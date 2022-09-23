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
    inventions*: int
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

  PopLike*[T] = concept c
    c[int] is Program[T]
    (c[int] < c[int]) is bool
    c.len is int
    for v in c:
      v is Program[T]

  LegacyPop[T] = PopLike[T] or Population[T]

iterator items*[T](q: HeapQueue[T]): T =
  ## helper for heapqueue-based populations
  for i in 0..<q.len:
    yield q[i]

# make sure we aren't breaking concepts 🙄
assert HeapQueue[Program[int]] is PopLike[int]
assert seq[Program[int]] is PopLike[int]
assert array[5, Program[int]] is PopLike[int]

proc parsimony*(pop: Population): float
proc parsimony*(pop: Population; ken: PopMetrics): float

proc toggleParsimony*(pop: Population; value = on) =
  ## turn parsimony `on` or `off`; when switching parsimony on,
  ## this will recompute and set the parsimony for the population
  pop.ken.parsimony =
    if value:
      parsimony(pop, pop.ken)
    else:
      NaN

proc resetParsimony*(pop: Population) {.deprecated.} =
  ## recompute and set the parsimony for the population,
  ## if parsimony is enabled for the population
  if not pop.ken.parsimony.isNaN:
    pop.ken.parsimony = parsimony(pop, pop.ken)

template learn(pop: Population; p: Program; pos: int) =
  when populationCache:
    pop.cache.incl p.hash
  if p.isValid:
    if p.score.isValid:
      pop.ken.scores.push p.score
    else:
      raise Defect.newException "program is valid, score isn't"
    pop.ken.validity.push 1.0
  else:
    pop.ken.validity.push 0.0
  pop.ken.lengths.push p.len.float
  if p.core == pop.ken.core:
    pop.ken.ages.push float(int p.generation)
  else:
    inc pop.ken.immigrants
  if Cached notin p.flags:
    inc pop.ken.inventions

template forget(population: Population; program: Program; pos: int) =
  when populationCache:
    population.cache.excl program.hash
  if program.isValid:
    if program.score.isValid:
      population.ken.scores.pop program.score
    population.ken.validity.pop 1.0
  else:
    population.ken.validity.pop 0.0
  population.ken.lengths.pop program.len.float
  if program.core == population.ken.core:
    population.ken.ages.pop float(int program.generation)
  else:
    dec population.ken.immigrants
  if Cached notin program.flags:
    dec population.ken.inventions

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

func best*(pop: Population): Score {.deprecated.} =
  ## the score of the fittest program in the population, or NaN
  if not pop.fittest.isNil:
    if pop.fittest.score.isValid:
      return pop.fittest.score
  return NaN.Score

func fittest*[T](pop: Population[T]): Program[T] {.deprecated.} =
  ## the fittest member of the population
  withInitialized pop:
    pop.fittest

proc rescale(ken: PopMetrics; score: Score): Score =
  ## rescale a given score according to the distribution of the population
  result =
    Score:
      sgn(score).float *
        abs(score.float / min(score.float, ken.scores.min.float))

proc penalizeSize(ken: PopMetrics; score: Score; length: int): Score =
  ## apply some pressure on program size
  if ken.parsimony.isNaN:
    result = score
  else:
    var s = rescale(ken, score).float
    if ken.parsimony < 0.0:  # length appears to be hurting scores
      # reduce the score of longer programs
      s += ken.parsimony * length.float
    else:                        # length appears to be helping scores
      # raise the score of longer programs
      s += ken.parsimony * length.float
    result = Score s

proc score*(ken: PopMetrics; score: Score; length: int): Score =
  ## adjust the score according to the population's parsimony and a length
  if ken.parsimony.isNaN:
    score
  elif score.isValid:
    penalizeSize(ken, score, length)
  else:
    Score NaN

proc score*(ken: PopMetrics; p: Program): Score =
  ## retrieve the parsimonious Score for Program `p`
  if ken.parsimony.isNaN:
    p.score
  elif p.isValid:
    penalizeSize(ken, p.score, p.len)
  else:
    Score NaN

template maybeReportFittest(pop: Population; p: Program) =
  when defined(greadReportFittestChanges):
    if not p.isNil:
      echo fmt"fittest in {pop.ken.core} score {p.score} from {p.core}/{p.generation}"

proc maybeResetFittest*[T](pop: Population[T]; p: Program[T]) =
  ## reset the fittest pointer if the argument is actually superior
  withInitialized pop:
    if p.isValid:
      block:
        if not pop.fittest.isNil:
          if pop.fittest.hash == p.hash:
            break
          # the fittest is not a function of parsimony
          if p <= pop.fittest:
            break
        maybeReportFittest(pop, pop.fittest)
        pop.fittest = p
        maybeReportFittest(pop, pop.fittest)
        p.flags.incl FinestKnown

template addImpl[T](population: Population[T]; p: Program[T]) =
  population.programs.add p
  learn(population, p, population.programs.high)

proc introduce*[T](population: Population[T]; p: Program[T]) =
  ## introduce a foreign program to the local pop without
  ## setting it as the fittest individual, etc.
  withInitialized population:
    when populationCache:
      if not population.cache.containsOrIncl p.hash:
        addImpl(population, p)
    else:
      addImpl(population, p)

proc add*[T](pop: Population[T]; p: Program[T]) =
  ## add a new program to the population
  withInitialized pop:
    if p.isNil:
      raise Defect.newException "nil program"
    elif pop.isNil:
      raise Defect.newException "nil pop"
    else:
      template optimizeAway: bool =
        when populationCache:
          pop.cache.containsOrIncl(p.hash)
        else:
          false
      if not optimizeAway():
        addImpl(pop, p)
        when not defined(greadFast):
          maybeResetFittest(pop, p)

iterator items*[T](pop: Population[T]): Program[T] =
  withInitialized pop:
    for p in pop.programs.items:
      yield p

iterator mitems*[T](pop: Population[T]): var Program[T] {.deprecated.} =
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

proc `[]`*[T](pop: Population[T]; index: int): Program[T] =
  ## retrieve a program via (unstable?) index
  pop.programs[index]

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

proc pop*[T](population: Population[T]): Program[T] =
  ## remove and return a member of the population;
  ## this does not reset population metrics
  withPopulated population:
    let index = population.programs.high
    result = population.programs[index]
    del(population, index)

proc randomRemoval*[T](pop: Population[T]; rng: var Rand): Program[T] =
  ## remove and return a random member of the population;
  ## this does not reset population metrics
  withPopulated pop:
    let query = pop.randomMember(rng)
    result = query.program
    del(pop, query.index)

proc parsimony*(pop: Population; ken: PopMetrics): float =
  ## compute parsimony for members of the population with valid scores
  withInitialized pop:
    if ken.scores.n == 0:
      return defaultParsimony
    var scores = newSeqOfCap[float](ken.scores.n)
    var lengths = newSeqOfCap[float](ken.scores.n)
    for i, p in pop.pairs:
      if p.isValid:
        scores.add rescale(ken, p.score)
        lengths.add p.len.float
    result = covariance(lengths, scores) / variance(lengths)

proc parsimony*(pop: Population): float {.deprecated.} =
  ## compute parsimony for members of the population with valid scores
  withInitialized pop:
    result = parsimony(pop, pop.ken)

proc nextGeneration*(pop: Population): Generation =
  ## inform the population that we're entering a new generation
  withInitialized pop:
    inc pop.ken.generation
    result = pop.ken.generation

func generations*(pop: Population): Generation =
  ## return the population's Generation
  withInitialized pop:
    pop.ken.generation

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

proc scoreChanged*(pop: Population; p: Program; s: Option[float]; index: int) =
  ## inform the population of a change to the score of `p` at `index`; this
  ## is used to update metrics, parsimony, and the `fittest` population member
  withInitialized pop:
    if p.isValid:
      pop.ken.validity.pop 1.0
      if p.score.isValid:
        if pop.ken.scores.n == 0:
          raise Defect.newException "fewer than zero scores?"
        pop.ken.scores.pop p.score
    else:
      pop.ken.validity.pop 0.0
    if s.isSome and s.get.isValid:
      p.score = get s
      pop.ken.validity.push 1.0
      pop.ken.scores.push p.score
      p.zombie = false  # NOTE: trigger a defect if necessary
      when not defined(greadFast):
        maybeResetFittest(pop, p)
    else:
      p.score = NaN
      p.zombie = true
      pop.ken.validity.push 0.0

proc core*(pop: Population): CoreSpec {.deprecated.} =
  withInitialized pop:
    pop.ken.core

proc resetMetrics*(pop: Population) =
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
