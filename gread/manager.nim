import std/options
import std/random
import std/hashes
import std/deques

import gread/population
import gread/tableau
import gread/fertilizer
import gread/spec
import gread/programs
import gread/primitives
import gread/data

type
  Manager*[T] = object
    platform: T
    tableau: Tableau
    fitness: Fitness[T]
    primitives: Primitives[T]
    population: Population[T]
    operators: AliasMethod[Operator[T]]

  AliasMethod*[T] = object
    prob: seq[float]
    alias: seq[int]
    data: seq[T]

  Operator*[T] = proc(pop: var Population[T]): Option[Program[T]] {.nimcall.}
  Weight = float or float64
  OperatorWeight*[T] = tuple[operator: Operator[T]; weight: float64]

proc newManager*[T](platform: T; tableau: Tableau; primitives: Primitives[T];
                    operators: openArray[(Operator[T], float64)] = @[];
                    fitness: Fitness[T] = nil): Manager[T] =
  result = Manager(platform: platform, tableau: tableau, primitives: primitives)
  initAliasMethod(result.operators, operators)

proc tableau*(man: Manager): Tableau = man.tableau

proc fittest*[T](man: Manager[T]): Program[T] =
  if not man.population.isNil:
    result = man.population.fittest

proc `primitives=`*[T](man: var Manager[T]; primitives: Primitives[T]) =
  man.primitives = primitives

proc `population=`*[T](man: var Manager[T]; population: Population[T]) =
  man.population = population

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

proc randomOperator*[T](man: Manager[T]): Operator[T] =
  if man.operators.len == 0:
    raise ValueError.newException "manager needs operators assigned"
  else:
    choose man.operators

proc `operators=`*[T](man: var Manager[T];
                      weighted: openArray[(Operator[T], float64)]) =
  initAliasMethod(man.operators, weighted)

proc fitness*[T](man: Manager[T]): Fitness[T] =
  ## get the current fitness function associated with the manager
  man.fitness

proc `fitness=`*[T](man: var Manager[T]; fitter: Fitness[T]) =
  ## assign a new fitness function to the manager
  man.fitness = fitter

proc score*[T](man: Manager[T]; p: var Program[T]): Score =
  ## assert the score on an individual program using the population
  if not p.zombie:
    if not p.score.isValid:
      p.score = man.fitness(man.platform, p)
      if p.score.isValid:
        p.score = penalizeSize(man.population, p.score.float, p.len)
  result = p.score

when false:
  proc score*[T](man: Manager[T]; p: Program[T]): Score =
    ## fetch or produce the score on an individual program using the population
    if p.zombie:
      NaN
    elif p.score.isValid:
      p.score
    else:
      var s = man.fitness(man.platform, p)
      s = penalizeSize(man.population, s.float, p.len)

proc randomPop*[T](man: Manager[T]): Population[T] =
  ## create a new (random) population using the given manager's parameters
  result = newPopulation(man.tableau.seedPopulation)
  while result.len < man.tableau.seedPopulation:
    result.add:
      randProgram(man.primitives, man.tableau.seedProgramSize)

proc refit*[T](man: Manager[T]) =
  ## re-score all members of the population
  for p in man.population.mitems:
    p.score = NaN
