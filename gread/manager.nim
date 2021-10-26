import std/options
import std/random
import std/hashes

import gread/population
import gread/tableau
import gread/fertilizer
import gread/spec
import gread/programs
import gread/primitives
import gread/data
import gread/aliasmethod

type
  Manager*[T] = object
    platform: T
    tableau: Tableau
    fitness: Fitness[T]
    primitives: Primitives[T]
    population: Population[T]
    operators: AliasMethod[Operator[T]]

  Operator*[T] = proc(pop: var Population[T]): Option[Program[T]] {.nimcall.}
  Weight = float or float64
  OperatorWeight*[T] = tuple[operator: Operator[T]; weight: float64]

proc tableau*(man: Manager): Tableau = man.tableau

proc fittest*[T](man: Manager[T]): Program[T] =
  if not man.population.isNil:
    result = man.population.fittest

proc `primitives=`*[T](man: var Manager[T]; primitives: Primitives[T]) =
  man.primitives = primitives

proc `population=`*[T](man: var Manager[T]; population: Population[T]) =
  man.population = population

proc newManager*[T](platform: T; tableau: Tableau; primitives: Primitives[T];
                    operators: openArray[(Operator[T], float64)] = @[];
                    fitness: Fitness[T] = nil): Manager[T] =
  result = Manager(platform: platform, tableau: tableau, primitives: primitives)
  initAliasMethod(result.operators, operators)

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
