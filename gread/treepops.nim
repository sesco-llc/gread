import std/algorithm
import std/random

import gread/aliasmethod
import gread/evolver
import gread/tableau
import gread/genotype

import trees/avl
export avl

type
  TreePop*[T] = AVLTree[(float, T), T]

proc initTreePop*[T](initialSize: Natural = 4): TreePop[T] =
  discard

proc push*[T](tree: var TreePop[T]; score: float; item: sink T) =
  tree.insert((score, item), item)

proc pop*[T](tree: var TreePop[T]): T =
  let bug = tree.select(1)
  result = bug.val
  tree.remove(bug.key)

proc del*[T](tree: var TreePop[T], index: Natural) =
  let bug = tree.select(index+1)
  tree.remove(bug.key)

proc high*[T](tree: TreePop[T]): Natural {.inline.} =
  tree.len - 1

iterator items*[T](tree: TreePop[T]): T =
  for key, val in tree.pairs:
    yield val

type
  TreeOperator*[T] = proc(rng: var Rand; population: TreePop[T]; size: int): seq[T] {.nimcall.}

  TreeEvolver*[T] = object of GenomeEvolver[T]
    population*: TreePop[T]

proc initEvolver*[T](evo: var TreeEvolver[T]; tableau: Tableau; rng: Rand = randState()) =
  ## perform initial setup of the Evolver, binding tableau
  initEvolver(evo.LeanEvolver, tableau, rng)

proc tournament*[T](evo: var TreeEvolver[T]; size: Positive;
                    order = Descending): T =
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"
  let index = tournament(evo.rng, evo.population.high, size, order = order)
  let bug = evo.population.select(1 + index)
  result = bug.val

proc tournament*[T](evo: var TreeEvolver[T]; order = Descending): T =
  tournament(evo, evo.tableau.tournamentSize, order = order)

proc evict*[T](rng: var Rand; population: var TreePop[T]; size: Positive): T =
  if population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"
  let index = tournament(rng, population.high, size, order = Descending)
  let bug = population.select(1+index)
  result = bug.val
  population.del(index)

proc remove*[T](rng: var Rand; population: var TreePop[T]; size: Positive;
                count: Positive = 1) =
  var count = count
  while count > 0:
    if population.len < 1:
      raise ValueError.newException:
        "cannot run a tournament with empty population"
    let index = tournament(rng, population.high, size, order = Descending)
    population.del(index)
    dec count

proc best*[T](population: TreePop[T]): T =
  if population.len == 0:
    raise ValueError.newException "population is empty"
  else:
    let bug = population.select(population.len)
    result = bug.val

proc randomMember*[T](evo: var TreeEvolver[T]): T =
  let bug = evo.population.select(1 + evo.rng.rand(evo.population.high))
  result = bug.val

proc sort*[T](population: var TreePop[T]) = discard

proc run*[T](evo: var TreeEvolver[T]; op: GenomeOperatorSpec[T]): GenomeGroup[T] =
  var group: GenomeGroup[T]
  group.add evo.tournament()
  group.add evo.tournament()
  op.measureOperator:
    result = op.fn(evo.rng, group)
