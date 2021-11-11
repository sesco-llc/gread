import std/sequtils
import std/deques
import std/heapqueue
import std/options
import std/random
import std/hashes
import std/sets

import pkg/adix/stat except variance

import gread/spec
import gread/programs
import gread/maths

const
  defaultParsimony = -0.01
  populationCache = false

type
  PopMetrics* = object
    core*: CoreSpec
    generation*: Generation
    lengths*: MovingStat[float32]
    scores*: MovingStat[float32]
    ages*: MovingStat[float32]
    immigrants*: int
    parsimony*: float
    validity*: MovingStat[float32]
    # the rest are populated JIT
    bestSize*: int
    bestGen*: Generation
    bestScore*: Score
    staleness*: float
    usurper*: CoreSpec
    size*: int

  Population*[T: ref] = ref object
    programs: seq[Program[T]]
    lengths: seq[float]
    scores: seq[float]
    fittest: Program[T]
    ken: PopMetrics
    when populationCache:
      cache: HashSet[Hash]

template learn(pop: Population; p: Program; pos: int) =
  when populationCache:
    pop.cache.incl p.hash
  if not p.zombie and p.score.isValid:
    pop.scores[pos] = float penalizeSize(pop, p.score, p.len)
    pop.lengths[pos] = float(p.len)
    pop.ken.scores.push float(p.score)
    pop.ken.validity.push 1.0
  else:
    pop.scores[pos] = NaN
    pop.lengths[pos] = NaN
    pop.ken.validity.push 0.0
  pop.ken.lengths.push float(p.len)
  if p.core == pop.ken.core:
    pop.ken.ages.push float(int p.generation)
  else:
    inc pop.ken.immigrants

template forget(pop: Population; p: Program; pos: int) =
  when populationCache:
    pop.cache.excl p.hash
  if not p.zombie and p.score.isValid:
    pop.scores[pos] = NaN
    pop.lengths[pos] = NaN
    if p.score.isValid:
      pop.ken.scores.pop float(p.score)
      pop.ken.validity.pop 1.0
    else:
      pop.ken.validity.pop 0.0
  pop.ken.lengths.pop float(p.len)
  if p.core == pop.ken.core:
    pop.ken.ages.pop float(int p.generation)
  else:
    dec pop.ken.immigrants
  if not pop.fittest.isNil:
    if p.hash == pop.fittest.hash:
      pop.fittest = nil

template withInitialized*(pop: Population; logic: untyped): untyped =
  if pop.isNil:
    raise Defect.newException "initialize your population first"
  else:
    logic

template withPopulated*(pop: Population; logic: untyped): untyped =
  withInitialized pop:
    if pop.len == 0:
      raise ValueError.newException "population is empty"
    else:
      logic

proc newPopulation*[T](size = 0; core = none int): Population[T] =
  result = Population[T](programs: newSeqOfCap[Program[T]](size))
  result.ken.core = core
  result.ken.parsimony = defaultParsimony

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
  withInitialized pop:
    pop.fittest

proc penalizeSize*(pop: Population; score: Score; length: int): Score =
  ## apply some pressure on program size
  withInitialized pop:
    var s = score.float
    if not pop.ken.parsimony.isNaN:
      if pop.ken.parsimony < 0.0:
        s += min(0.0, pop.ken.parsimony * length.float)
    result = Score s

proc maybeResetFittest[T](pop: Population[T]; p: Program[T]) =
  ## reset the fittest pointer if the argument is actually superior
  withInitialized pop:
    if p.score.isValid:
      if pop.fittest.isNil or pop.fittest.score < p.score:
        pop.fittest = p
        p.flags.incl FinestKnown

template growNaNs(pop: Population; field: typed): untyped =
  while pop.programs.len > field.len:
    setLen(field, field.len + 1)
    field[field.high] = NaN

template addImpl[T](pop: Population[T]; p: Program[T]) =
  pop.programs.add p
  growNaNs(pop, pop.scores)
  growNaNs(pop, pop.lengths)
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
    when populationCache:
      if not pop.cache.containsOrIncl p.hash:
        addImpl(pop, p)
        maybeResetFittest(pop, p)
    else:
      addImpl(pop, p)
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

type
  IndexedProgram[T] = tuple
    index: int
    program: Program[T]

proc randomMember*[T](pop: Population[T]): IndexedProgram[T] =
  ## return a random member of the population
  withPopulated pop:
    let index = rand pop.programs.high
    result = (index: index, program: pop.programs[index])

proc del*[T](pop: Population[T]; index: int) =
  ## remove a specific member of the population
  withPopulated pop:
    pop.forget(pop.programs[index], index)
    del(pop.programs, index)

proc randomRemoval*[T](pop: Population[T]): Program[T] =
  ## remove a random member of the population
  withPopulated pop:
    del(pop, rand pop.programs.high)

proc selectNonNaN[T](a: openArray[T]): seq[float] =
  result = newSeqOfCap[float](a.len)
  for s in a.items:
    if not float(s).isNaN:
      result.add float(s)

proc parsimony*(pop: Population): float =
  ## compute parsimony for members of the population with valid scores
  withInitialized pop:
    var scores = selectNonNaN(pop.scores)
    var lengths = selectNonNaN(pop.lengths)
    result =
      if scores.len > 0:
        -covariance(lengths, scores) / variance(lengths)
      else:
        defaultParsimony
    pop.ken.parsimony = result

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

proc scoreChanged*(pop: Population; p: Program; s: Option[Score]; index: int) =
  withInitialized pop:
    if s.isSome:
      pop.scores[index] = penalizeSize(pop, get s, p.len).float
      # the lengths could be NaN for this program if it was
      # entered without a valid score... or something.
      pop.lengths[index] = p.len.float
      p.score = get s
      p.zombie = false
      maybeResetFittest(pop, p)
    else:
      pop.scores[index] = NaN
      pop.lengths[index] = NaN
      p.score = NaN
      p.zombie = true

when false:
  proc penalized*(pop: Population; index: int): Score =
    withInitialized pop:
      Score pop.scores[index]

proc core*(pop: Population): CoreSpec =
  withInitialized pop:
    pop.ken.core

proc resetScoreMetrics(pop: Population) =
  withInitialized pop:
    clear pop.ken.validity
    clear pop.ken.scores
    for p in pop.items:
      if p.score.isValid:
        pop.ken.validity.push 1.0
        pop.ken.scores.push p.score
      else:
        pop.ken.validity.push 0.0

proc metrics*(pop: Population): PopMetrics =
  ## returns a copy of the population's metrics
  resetScoreMetrics pop
  result = pop.ken
  result.size = pop.len
  if not pop.fittest.isNil:
    result.bestSize = pop.fittest.len
    result.bestScore = pop.fittest.score
    result.bestGen = pop.fittest.generation
    if pop.fittest.core == pop.ken.core:
      let current = pop.ken.generation.int.float
      result.staleness = (current - pop.fittest.generation.float) / current
      result.usurper = none int
    else:
      result.staleness = NaN
      result.usurper = pop.fittest.core
