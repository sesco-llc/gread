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
    cache: HashSet[Hash]
    programs: seq[Program[T]]
    lengths: seq[float]
    scores: seq[float]
    fittest: Program[T]
    ken: PopMetrics

template learn(pop: Population; p: Program; pos: int) =
  pop.cache.incl p.hash
  if p.isValid:
    pop.scores[pos] = float(p.score)
    pop.lengths[pos] = float(p.len)
    if p.isValid and p.score.isValid:
      pop.ken.scores.push float(p.score)
      pop.ken.validity.push 1.0
    else:
      pop.ken.validity.push 0.0
  pop.ken.lengths.push float(p.len)
  if p.core == pop.ken.core:
    pop.ken.ages.push float(int p.generation)
  else:
    inc pop.ken.immigrants

template forget(pop: Population; p: Program; pos: int) =
  pop.cache.excl p.hash
  if p.isValid:
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
  pop.fittest

proc penalizeSize*(pop: Population; score: float; length: int): Score =
  # apply some pressure on program size
  var s = score
  if not pop.ken.parsimony.isNaN:
    s += min(0.0, pop.ken.parsimony * length.float)
  result = Score s

proc maybeResetFittest[T](pop: Population[T]; p: Program[T]) =
  ## reset the fittest pointer if the argument is actually superior
  if p.score.isValid:
    if pop.fittest.isNil or pop.fittest.score < p.score:
      pop.fittest = p

template growNaNs(pop: Population; field: typed): untyped =
  while pop.programs.len > field.len:
    setLen(field, field.len + 1)
    field[field.high] = NaN

proc introduce*[T](pop: Population[T]; p: Program[T]) =
  ## introduce a foreign program to the local pop without
  ## setting it as the fittest individual, etc.
  if not pop.cache.containsOrIncl p.hash:
    pop.programs.add p
    growNaNs(pop, pop.scores)
    growNaNs(pop, pop.lengths)
    pop.learn(p, pop.programs.high)

proc add*[T](pop: Population[T]; p: Program[T]) =
  ## add a new program to the population
  if not pop.cache.containsOrIncl p.hash:
    pop.introduce p
    maybeResetFittest(pop, p)

type
  TopPop[T] = tuple[score: Score, program: Program[T]]

iterator top*[T](pop: Population[T]; n: int): TopPop[T] =
  ## iterate through the best performers, low-to-high
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
  for p in pop.programs.items:
    yield p

iterator mitems*[T](pop: Population[T]): var Program[T] =
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

proc unfit*(pop: Population) =
  ## mark all members of the population as unscored
  raise Defect.newException "this no longer makes sense"
  for p in pop.mitems:
    # critically, when you reset everyone's scores, make sure
    # you at least try to reset the score of the fittest program
    # FIXME: for now we just don't reset the fittest individual
    if cast[int](p) != cast[int](pop.fittest):
      p.score = NaN

proc selectNonNaN[T](a: openArray[T]): seq[float] =
  result = newSeqOfCap[float](a.len)
  for s in a.items:
    if not float(s).isNaN:
      result.add float(s)

proc parsimony*(pop: Population): float =
  ## compute parsimony for members of the population with valid scores
  var scores = selectNonNaN(pop.scores)
  var lengths = selectNonNaN(pop.lengths)
  result =
    if scores.len > 0:
      -covariance(lengths, scores) / variance(lengths)
    else:
      defaultParsimony
  pop.ken.parsimony = result
  unfit pop

proc nextGeneration*(pop: Population): Generation =
  ## inform the population that we're entering a new generation
  inc pop.ken.generation
  result = pop.ken.generation

func generations*(pop: Population): Generation =
  ## return the population's Generation
  pop.ken.generation

proc contains*(pop: Population; p: Program): bool =
  ## true if the `pop` contains Program `p`
  p.hash in pop.cache

proc scoreChanged*(pop: Population; p: Program; s: Score; index = none int) =
  if index.isSome:
    pop.scores[get index] = float s
  else:
    for i, q in pop.programs.pairs:
      if q.hash == p.hash:
        scoreChanged(pop, p, s, index = some i)
        break
    raise ValueError.newException:
      "program does not exist in this population"

proc core*(pop: Population): CoreSpec = pop.ken.core

proc metrics*(pop: Population): PopMetrics =
  ## returns a copy of the population's metrics
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
