import std/sequtils
import std/deques
import std/heapqueue
import std/options
import std/random
import std/hashes
import std/packedsets
import std/math

import pkg/adix/stat except variance

import gread/spec
import gread/programs
import gread/maths

const
  defaultParsimony = 0.01
  #[

  this is quite a bit slower when enabled, but that's probably because
  the population is not swamped with programs that are comprised of just
  a single terminal, etc. the greater average program size is what makes
  larger populations more valuable.

  ]#
  populationCache = true

type
  PopMetrics* = object
    core*: CoreSpec
    generation*: Generation
    lengths*: MovingStat[float32]       ## accurate only after resetMetrics()
    scores*: MovingStat[float64]
    ages*: MovingStat[float32]
    immigrants*: int
    parsimony*: float
    validity*: MovingStat[float32]
    caches*: MovingStat[float32]
    # the rest are populated JIT
    bestSize*: int
    bestGen*: Generation
    bestScore*: Score
    staleness*: float
    usurper*: CoreSpec
    size*: int

  Population*[T: ref] = ref object
    programs: seq[Program[T]]
    fittest: Program[T]
    ken: PopMetrics
    when populationCache:
      cache: PackedSet[Hash]

proc parsimony*(pop: Population): float

proc toggleParsimony*(pop: Population; value = on) =
  ## turn parsimony `on` or `off`; when switching parsimony on,
  ## this will recompute and set the parsimony for the population
  pop.ken.parsimony =
    if value:
      parsimony pop
    else:
      NaN

func usesParsimony*(pop: Population): bool =
  ## true if the population uses parsimony in its score() routines
  not pop.ken.parsimony.isNaN

proc resetParsimony*(pop: Population) =
  ## recompute and set the parsimony for the population,
  ## if parsimony is enabled for the population
  if pop.usesParsimony:
    profile "reset parsimony":
      pop.ken.parsimony = parsimony pop

template learn(pop: Population; p: Program; pos: int) =
  when populationCache:
    pop.cache.incl p.hash
  if p.isValid:
    if p.score.isValid:
      pop.ken.scores.push p.score
    else:
      raise
    pop.ken.validity.push 1.0
  else:
    pop.ken.validity.push 0.0
  pop.ken.lengths.push p.len.float
  if p.core == pop.ken.core:
    pop.ken.ages.push float(int p.generation)
  else:
    inc pop.ken.immigrants

template forget(pop: Population; p: Program; pos: int) =
  when populationCache:
    pop.cache.excl p.hash
  if p.isValid:
    if p.score.isValid:
      pop.ken.scores.pop p.score
    pop.ken.validity.pop 1.0
  else:
    pop.ken.validity.pop 0.0
  pop.ken.lengths.pop p.len.float
  if p.core == pop.ken.core:
    pop.ken.ages.pop float(int p.generation)
  else:
    dec pop.ken.immigrants
  # don't remove the reference to the fittest individual
  when false:
    if not pop.fittest.isNil:
      # FIXME: ehhh this is wrong in the event there are dupes in the pop
      #        (we could check populationCache, but should we even do this?)
      if p.hash == pop.fittest.hash:
        pop.fittest = nil

template withInitialized*(pop: Population; logic: untyped): untyped =
  ## execute the body only when the population is initialized
  if pop.isNil:
    raise Defect.newException "initialize your population first"
  else:
    logic

template withPopulated*(pop: Population; logic: untyped): untyped =
  ## execute the body only when the population has members
  withInitialized pop:
    if pop.len == 0:
      raise ValueError.newException "population is empty"
    else:
      logic

proc newPopulation*[T](size = 0; core = none int): Population[T] =
  ## create a new population with the given capacity and core
  result = Population[T](programs: newSeqOfCap[Program[T]](size))
  result.ken.core = core
  result.ken.parsimony = NaN

func len*[T](p: Population[T]): int =
  ## the number of programs in the population
  p.programs.len

func best*(pop: Population): Score =
  ## the score of the fittest program in the population, or NaN
  if not pop.fittest.isNil:
    if pop.fittest.score.isValid:
      return pop.fittest.score
  return NaN.Score

func fittest*[T](pop: Population[T]): Program[T] =
  ## the fittest member of the population
  withInitialized pop:
    pop.fittest

proc rescale*(pop: Population; score: Score; outlier: Score): Score =
  ## rescale a given score according to the distribution of the population;
  ## the outlier score may not be recorded in the population...
  result =
    Score:
      sgn(score).float * abs(score.float /
           min(score.float,
               min(outlier.float,
                   pop.ken.scores.min.float)))

proc rescale*(pop: Population; score: Score): Score =
  ## rescale a given score according to the distribution of the population
  result = rescale(pop, score, score)

proc penalizeSize(pop: Population; score: Score; length: int): Score =
  ## apply some pressure on program size
  withInitialized pop:
    var s = rescale(pop, score).float
    if pop.usesParsimony:
      if pop.ken.parsimony < 0.0:  # length appears to be hurting scores
        # reduce the score of longer programs
        s += pop.ken.parsimony * length.float
      else:                        # length appears to be helping scores
        # raise the score of longer programs
        s += pop.ken.parsimony * length.float
    result = Score s

proc score*(pop: Population; score: Score; length: int;
            outlier: Score): Score =
  ## adjust the score according to the population's parsimony and a length
  if score.isValid:
    penalizeSize(pop, score, length)
  else:
    Score NaN

proc score*(pop: Population; score: Score; length: int): Score =
  ## adjust the score according to the population's parsimony and a length
  if score.isValid:
    penalizeSize(pop, score, length)
  else:
    Score NaN

proc score*(pop: Population; p: Program): Score =
  ## retrieve the parsimonious Score for Program `p`
  if p.isValid:
    penalizeSize(pop, p.score, p.len)
  else:
    Score NaN

proc maybeResetFittest*[T](pop: Population[T]; p: Program[T]) =
  ## reset the fittest pointer if the argument is actually superior
  withInitialized pop:
    if p.isValid:
      block:
        if not pop.fittest.isNil:
          if pop.fittest.hash == p.hash:
            break
          # the fittest is not a function of parsimony
          if pop.fittest >= p:
            break
        pop.fittest = p
        p.flags.incl FinestKnown

template addImpl[T](pop: Population[T]; p: Program[T]) =
  pop.programs.add p
  learn(pop, p, pop.programs.high)

proc introduce*[T](pop: Population[T]; p: Program[T]) =
  ## introduce a foreign program to the local pop without
  ## setting it as the fittest individual, etc.
  withInitialized pop:
    when populationCache:
      if not pop.cache.containsOrIncl p.hash:
        addImpl(pop, p)
    else:
      addImpl(pop, p)

proc add*[T](pop: Population[T]; p: Program[T]) =
  ## add a new program to the population
  withInitialized pop:
    if p.isNil:
      raise Defect.newException "nil program"
    elif pop.isNil:
      raise Defect.newException "nil pop"
    else:
      when populationCache:
        if not pop.cache.containsOrIncl p.hash:
          addImpl(pop, p)
          when not defined(greadFast):
            maybeResetFittest(pop, p)
      else:
        addImpl(pop, p)
        when not defined(greadFast):
          maybeResetFittest(pop, p)

type
  TopPop[T] = tuple[score: Score, program: Program[T]]

iterator top*[T](pop: Population[T]; n: int): TopPop[T] =
  ## iterate through the best performers, low-to-high
  withInitialized pop:
    var queue: HeapQueue[TopPop[T]]
    var valid = false
    for i in 0..<pop.programs.len:
      var p = pop.programs[i]
      if p.isValid or not valid:
        valid = valid or p.score.isValid
        # handling missing scores
        queue.push (score: p.score, program: p)
      # discard invalid stuff if we have valid scores in the queue
      while valid and queue.len > 0 and not queue[0].score.isValid:
        discard queue.pop()
      # discard excess queue members
      while queue.len > n:
        discard queue.pop()
    while queue.len > 0:
      yield queue.pop()

proc first*[T](pop: Population[T]): Program[T] =
  ## return the fittest individual in the population
  withPopulated pop:
    if pop.fittest.isNil:
      # i guess
      for program in pop.top(1):
        return program
    else:
      return pop.fittest

iterator items*[T](pop: Population[T]): Program[T] =
  withInitialized pop:
    for p in pop.programs.items:
      yield p

iterator mitems*[T](pop: Population[T]): var Program[T] =
  withInitialized pop:
    for p in pop.programs.mitems:
      yield p

iterator pairs*[T](pop: Population[T]): (int, Program[T]) =
  var i: int
  for p in pop.items:
    yield (i, p)
    inc i

type
  IndexedProgram[T] = tuple
    index: int
    program: Program[T]

proc randomMember*[T](pop: Population[T]; rng: var Rand): IndexedProgram[T] =
  ## return a random member of the population
  withPopulated pop:
    let index = rng.rand pop.programs.high
    result = (index: index, program: pop.programs[index])

proc del*[T](pop: Population[T]; index: int) =
  ## remove a specific member of the population
  withPopulated pop:
    pop.forget(pop.programs[index], index)
    del(pop.programs, index)

proc randomRemoval*[T](pop: Population[T]; rng: var Rand): Program[T] =
  ## remove a random member of the population
  withPopulated pop:
    let query = pop.randomMember(rng)
    result = query.program
    del(pop, query.index)

proc parsimony*(pop: Population): float =
  ## compute parsimony for members of the population with valid scores
  withInitialized pop:
    if pop.ken.scores.n == 0:
      return defaultParsimony
    var scores = newSeqOfCap[float](pop.ken.scores.n)
    var lengths = newSeqOfCap[float](pop.ken.scores.n)
    for i, p in pop.pairs:
      if p.isValid:
        scores.add pop.rescale(p.score)
        lengths.add p.len.float
    result = covariance(lengths, scores) / variance(lengths)

proc nextGeneration*(pop: Population): Generation =
  ## inform the population that we're entering a new generation
  withInitialized pop:
    inc pop.ken.generation
    result = pop.ken.generation

func generations*(pop: Population): Generation =
  ## return the population's Generation
  withInitialized pop:
    pop.ken.generation

when populationCache:
  proc contains*(pop: Population; p: Program): bool =
    ## true if the `pop` contains Program `p`
    withInitialized pop:
      p.hash in pop.cache

template qualityTrackIt*(pop: Population; p: Program; s: Score;
                         body: untyped): untyped =
  withInitialized pop:
    if p.isValid and s.isValid:
      pop.ken.scores.pop s
    var it {.inject.}: Score = NaN
    body
    if it.isValid:
      pop.ken.scores.push it
      when not defined(greadFast):
        maybeResetFittest(pop, p)

template qualityTrackIt*(pop: Population; p: Program; s: Option[Score];
                         body: untyped): untyped =
  let score: Score =
    if s.isSome:
      get s
    else:
      NaN
  qualityTrackIt(pop, p, score):
    body

proc scoreChanged*[V](pop: Population; p: Program; s: Option[V]; index: int) =
  ## inform the population of a change to the score of `p` at `index`; this
  ## is used to update metrics, parsimony, and the `fittest` population member
  withInitialized pop:
    if p.isValid:
      pop.ken.validity.pop 1.0
      if p.score.isValid:
        pop.ken.scores.pop p.score
        assert pop.ken.scores.n >= 0
    else:
      pop.ken.validity.pop 0.0
    if s.isSome and s.get.isValid:
      p.score = strength(get s)
      pop.ken.validity.push 1.0
      pop.ken.scores.push p.score
      p.zombie = false  # NOTE: trigger a defect if necessary
      when not defined(greadFast):
        maybeResetFittest(pop, p)
    else:
      p.score = NaN
      p.zombie = true
      pop.ken.validity.push 0.0

proc core*(pop: Population): CoreSpec =
  withInitialized pop:
    pop.ken.core

proc resetMetrics(pop: Population) =
  ## reset validity, score, and parsimony metrics in the population; O(n)
  withInitialized pop:
    clear pop.ken.validity
    clear pop.ken.scores
    clear pop.ken.caches
    clear pop.ken.lengths
    for p in pop.items:
      if p.isValid:
        pop.ken.validity.push 1.0
        if p.score.isValid:
          pop.ken.scores.push p.score
      else:
        pop.ken.validity.push 0.0
      pop.ken.lengths.push p.len.float
      pop.ken.caches.push p.cacheSize.float
    resetParsimony pop

proc metrics*(pop: Population): PopMetrics =
  ## returns a copy of the population's metrics
  resetMetrics pop
  result = pop.ken
  result.size = pop.len
  if not pop.fittest.isNil:
    result.bestSize = pop.fittest.len
    result.bestScore = pop.fittest.score
    result.bestGen = pop.fittest.generation
    if pop.fittest.core == pop.ken.core:
      let current = pop.ken.generation.int.float
      result.staleness = pop.fittest.generation.float / current
      result.usurper = none int
    else:
      result.staleness = NaN
      result.usurper = pop.fittest.core
