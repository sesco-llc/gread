import std/deques
import std/heapqueue
import std/options
import std/random
import std/hashes
import std/sets

import pkg/adix/stat except variance

import gread/tableau
import gread/fertilizer
import gread/spec
import gread/programs
import gread/primitives
import gread/maths

const
  defaultParsimony = -0.02

type
  Population*[T: ref] = ref object
    platform: T
    tableau: Tableau
    cache: HashSet[Hash]
    primitives: Primitives[T]
    programs: seq[Program[T]]
    pcoeff: float
    fitness: Fitness[T]
    fittest: Program[T]
    generation: Generation
    operators: AliasMethod[Operator[T]]

  AliasMethod*[T] = object
    prob: seq[float]
    alias: seq[int]
    data: seq[T]

  Operator*[T] = proc(pop: var Population[T]): Option[Program[T]] {.nimcall.}
  Weight = float or float64
  OperatorWeight*[T] = tuple[operator: Operator[T]; weight: float64]

func tableau*(pop: Population): Tableau = pop.tableau

template ignore(pop: var Population; p: Program) {.used.} =
  pop.cache.incl p.hash

template forget(pop: var Population; p: Program) {.used.} =
  pop.cache.excl p.hash

proc primitives*[T](pop: Population[T]): Primitives[T] = pop.primitives

template withInitialized*(pop: Population; logic: untyped): untyped =
  if pop.isNil:
    raise Defect.newException "initialize your population first"
  else:
    if pop.primitives.isNil:
      raise Defect.newException "initialize your primitives first"
    else:
      logic

template withPopulated*(pop: Population; logic: untyped): untyped =
  withInitialized pop:
    if pop.len == 0:
      raise ValueError.newException "population is empty"
    else:
      logic

# vose alias method per https://en.wikipedia.org/wiki/Alias_method
proc initAliasMethod[T](am: var AliasMethod; input: openArray[(Operator[T], Weight)]) =
  var n = input.len
  setLen(am.prob, n)
  setLen(am.alias, n)
  setLen(am.data, n)

  # rescale the probabilities according to the quantity
  var operators = newSeqOfCap[OperatorWeight[T]](n)
  for (op, weight) in input.items:
    operators.add (op, weight * n.float)

  # sort the ops into deques of indices, according probability
  var small, large: Deque[int]
  for i, pair in operators.pairs:
    am.data[i] = pair.operator
    if pair.weight < 1.0:
      small.addLast i
    else:
      large.addLast i

  # consume and accumulate weights
  while small.len > 0 and large.len > 0:
    # consume
    let (l, g) = (popFirst small, popFirst large)
    am.prob[l] = operators[l].weight
    am.alias[l] = g
    # accumulate remainder
    operators[g].weight += operators[l].weight-1.0
    if operators[g].weight < 1.0:
      small.addLast g
    else:
      large.addLast g

  # consume the remaining
  while large.len > 0:
    let g = popFirst large
    am.prob[g] = 1.0

  while small.len > 0:
    let l = popFirst small
    am.prob[l] = 1.0

proc len(am: AliasMethod): int = am.data.len

proc choose[T](am: AliasMethod[T]): T =
  ## weighted random choice from the list of operators
  let i = rand(am.prob.len-1)
  `[]`(am.data):
    if rand(1.0) < am.prob[i]:
      i
    else:
      am.alias[i]

proc randomOperator*[T](pop: Population[T]): Operator[T] =
  if pop.operators.len == 0:
    raise ValueError.newException "population needs operators assigned"
  else:
    choose pop.operators

proc newPopulation*[T](platform: T; tab: Tableau; primitives: Primitives[T];
                       size = 0): Population[T] =
  if primitives.isNil:
    raise ValueError.newException "initialize your primitives"
  else:
    result = Population[T](tableau: tab, primitives: primitives,
                           platform: platform,
                           programs: newSeqOfCap[Program[T]](size))
    result.pcoeff =
      if tab.useParsimony:
        NaN
      else:
        defaultParsimony

proc `operators=`*[T](pop: var Population[T];
                      weighted: openArray[(Operator[T], float64)]) =
  initAliasMethod(pop.operators, weighted)

func len*[T](p: Population[T]): int =
  ## the number of programs in the population
  p.programs.len

proc fitness*[T](pop: Population[T]): Fitness[T] =
  ## get the current fitness function associated with the population
  pop.fitness

proc `fitness=`*[T](pop: var Population[T]; fitter: Fitness[T]) =
  ## assign a new fitness function for the population
  pop.fitness = fitter

func best*(pop: Population): Score =
  ## the score of the fittest program in the population, or NaN
  if not pop.fittest.isNil:
    if pop.fittest.score.isValid:
      return pop.fittest.score
  return NaN.Score

func fittest*[T](pop: Population[T]): Program[T] =
  pop.fittest

proc penalizeSize(pop: Population; score: float; length: int): Score =
  # apply some pressure on program size
  var s = score
  if not pop.pcoeff.isNaN:
    s += min(0.0, pop.pcoeff * length.float)
  result = Score s

proc score*[T](pop: Population[T]; p: var Program[T]): Score =
  ## assert the score on an individual program using the population
  if not p.zombie:
    if not p.score.isValid:
      p.score = pop.fitness(pop.platform, p)
      if p.score.isValid:
        p.score = pop.penalizeSize(p.score.float, p.len)
  result = p.score

proc score*[T](pop: Population[T]; p: Program[T]): Score =
  ## fetch or produce the score on an individual program using the population
  if p.zombie:
    NaN
  elif p.score.isValid:
    p.score
  else:
    var s = pop.fitness(pop.platform, p)
    pop.lengthPenalty(s.float, p.len)

proc maybeResetFittest[T](pop: var Population[T]; p: Program[T]) =
  ## reset the fittest pointer if the argument is actually superior
  if p.score.isValid:
    if pop.fittest.isNil:
      pop.fittest = p
    else:
      if not pop.fittest.score.isValid:
        pop.fittest.score = score(pop, pop.fittest)
      if not pop.fittest.score.isValid:
        pop.fittest = p
      elif p.score.isValid:  # i know.
        if pop.fittest.score < p.score:
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

proc randomPop*[T](p: T; tab: Tableau; c: Primitives[T]): Population[T] =
  ## produce a random population from the given tableau and primitives
  result = newPopulation(p, tab, c, tab.seedPopulation)
  while result.len < tab.seedPopulation:
    result.add:
      randProgram(c, tab.seedProgramSize)

when false:
  proc sort[T](pop: var Population[T]; fitness: Fitness[T];
               order = Descending) {.deprecated.} =
    ## too expensive
    for p in pop.programs.mitems:
      discard score(pop, p)
    proc fittest(a, b: Program[T]): int {.closure.} =
      cmp(a.score, b.score)
    sort(pop.programs, fittest, order = order)

type
  TopPop[T] = tuple[score: Score, program: Program[T]]

iterator top*[T](pop: Population[T]; n: int): TopPop[T] =
  ## iterate through the best performers, low-to-high
  var queue: HeapQueue[TopPop[T]]
  var valid = false
  for i in 0..<pop.programs.len:
    var p = pop.programs[i]
    if score(pop, p).isValid or not valid:
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
      result = pop.fittest

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
    let index = rand pop.programs.len-1
    result = (index: index, program: pop.programs[index])

proc del*[T](pop: var Population[T]; index: int) =
  ## remove a specific member of the population
  withPopulated pop:
    pop.forget pop.programs[index]
    del(pop.programs, index)

proc randomRemoval*[T](pop: var Population[T]): Program[T] =
  ## remove a random member of the population
  withPopulated pop:
    del(pop, rand pop.programs.len-1)

proc refit*(pop: var Population) =
  ## re-score all members of the population
  for p in pop.mitems:
    p.score = NaN
    discard score(pop, p)

proc unfit*(pop: var Population) =
  ## mark all members of the population as unscored
  for p in pop.mitems:
    p.score = NaN
  # critically, when you reset everyone's scores, make sure
  # you at least try to reset the score of the fittest program
  if not pop.fittest.isNil:
    discard score(pop, pop.fittest)

proc parsimony*(pop: Population): float =
  ## compure parsimony for members of the population with valid scores
  var lengths = newSeqOfCap[float](pop.len)
  var scores = newSeqOfCap[float](pop.len)
  for p in pop.items:
    let s = p.score #score(pop, p)
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
  for p in pop.mitems:
    discard score(pop, p)
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
