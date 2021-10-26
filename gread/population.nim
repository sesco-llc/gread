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
  Population*[T: ref] = ref object
    cache: HashSet[Hash]
    programs: seq[Program[T]]
    pcoeff: float
    generation: Generation
    fittest: Program[T]

template ignore(pop: var Population; p: Program) {.used.} =
  pop.cache.incl p.hash

template forget(pop: var Population; p: Program) {.used.} =
  pop.cache.excl p.hash

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

proc newPopulation*[T](size = 0): Population[T] =
  result = Population[T](programs: newSeqOfCap[Program[T]](size))
  result.pcoeff =
    if tab.useParsimony:
      NaN
    else:
      defaultParsimony

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
  if not pop.pcoeff.isNaN:
    s += min(0.0, pop.pcoeff * length.float)
  result = Score s

proc maybeResetFittest[T](pop: var Population[T]; p: Program[T]) =
  ## reset the fittest pointer if the argument is actually superior
  if p.score.isValid:
    if pop.fittest.isNil or pop.fittest.score < p.score:
      pop.fittest = p

proc introduce*[T](pop: var Population[T]; p: Program[T]) =
  ## introduce a foreign program to the local pop without
  ## setting it as the fittest individual, etc.
  if not pop.cache.containsOrIncl p.hash:
    pop.programs.add p

proc add*[T](pop: var Population[T]; p: Program[T]) =
  ## add a new program to the population
  if not pop.cache.containsOrIncl p.hash:
    pop.programs.add p
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

iterator mitems*[T](pop: var Population[T]): var Program[T] =
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

proc del*[T](pop: var Population[T]; index: int) =
  ## remove a specific member of the population
  withPopulated pop:
    pop.forget pop.programs[index]
    del(pop.programs, index)

proc randomRemoval*[T](pop: var Population[T]): Program[T] =
  ## remove a random member of the population
  withPopulated pop:
    del(pop, rand pop.programs.high)

proc unfit*(pop: var Population) =
  ## mark all members of the population as unscored
  for p in pop.mitems:
    # critically, when you reset everyone's scores, make sure
    # you at least try to reset the score of the fittest program
    # FIXME: for now we just don't reset the fittest individual
    if cast[int](p) != cast[int](pop.fittest):
      p.score = NaN

proc parsimony*(pop: Population): float =
  ## compute parsimony for members of the population with valid scores
  var lengths = newSeqOfCap[float](pop.len)
  var scores = newSeqOfCap[float](pop.len)
  for p in pop.items:
    let s = p.score
    if s.isValid:
      lengths.add p.len.float
      scores.add s.float
  result =
    if scores.len > 0:
      -covariance(lengths, scores) / variance(lengths)
    else:
      defaultParsimony

proc parsimony*(pop: var Population): float =
  ## calculate the parsimony and store it in the population; mark
  ## all members of the population as needing a new fitness score
  let p = pop
  result = p.parsimony
  pop.pcoeff = result
  unfit pop

proc nextGeneration*(pop: var Population): Generation =
  ## inform the population that we're entering a new generation
  inc pop.generation
  result = pop.generation

func generations*(pop: Population): Generation =
  ## return the population's Generation
  pop.generation

func pcoeff*(pop: Population): float =
  ## return the current parsimony coefficient
  pop.pcoeff

proc contains*(pop: Population; p: Program): bool =
  ## true if the `pop` contains Program `p`
  p.hash in pop.cache
