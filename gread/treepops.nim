import std/algorithm
import std/hashes
import std/options
import std/random

import pkg/adix/stat except Option

import pkg/trees/avl
export avl

import gread/aliasmethod
import gread/evolver
import gread/genotype
import gread/parsimony
import gread/population
import gread/tableau
export parsimony

type
  TreePop[T] = AVLTree[Shim[T], T]

  TreeParsimony*[T] = object of Parsimony[TreePop[T]]

  Shim[T] = object
    score: Option[float]
    data: Hash

proc `<`*[T](a, b: Shim[T]): bool =
  mixin `<`
  (downscale(a.score), a.data.hash) < (downscale(b.score), b.data.hash)

proc `==`*[T](a, b: Shim[T]): bool =
  a.data == b.data

proc score*[T](population: TreePop[T]; item: T): Option[float] =
  mixin score
  result = item.score

proc sort*[T](parsimony: var Parsimony[TreePop[T]]) =
  var stack = newSeqOfCap[T](parsimony.members.len)
  while parsimony.members.len > 0:
    let bug = parsimony.members.max
    parsimony.members.pop(bug.key)
    stack.add bug.val
  while stack.len > 0:
    let item = pop stack
    let shim = Shim[T](score: parsimony.score(item), data: item.hash)
    parsimony.members.insert(shim, item)

proc rank*[T](parsimony: TreeParsimony[T]; item: T): Positive =
  let shim = Shim[T](score: parsimony.score(item), data: item.hash)
  result = parsimony.members.rank(shim)

iterator items*[T](parsimony: TreeParsimony[T]): T =
  for item in parsimony.members.values:
    yield item

proc insert*[T](population: var TreePop[T]; s: Option[float]; item: T): bool =
  var shim = Shim[T](score: s, data: item.hash)
  result = population.insert(shim, item)

proc remove*[T](population: var TreePop[T]; s: Option[float]; item: T): bool =
  var shim = Shim[T](score: s, data: item.hash)
  result = population.remove(shim)

type
  TreeEvolver*[T] = object of GenomeEvolver[T]
    population*: TreeParsimony[T]

proc initEvolver*[T](evo: var TreeEvolver[T]; tableau: Tableau; rng: Rand = randState()) =
  ## perform initial setup of the Evolver, binding tableau
  initEvolver(evo.LeanEvolver, tableau, rng)

proc tournament*[T](evo: var TreeEvolver[T]; size: Positive;
                    order = Descending): T =
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"
  let index = tournament(evo.rng, evo.population.high, size, order = order)
  result = evo.population.select(1 + index)

proc tournament*[T](evo: var TreeEvolver[T]; order = Descending): T =
  tournament(evo, evo.tableau.tournamentSize, order = order)

proc evict*[T](evo: var TreeEvolver[T]): T =
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"
  else:
    let index = tournament(evo.rng, evo.population.high,
                           evo.tableau.tournamentSize,
                           order = Ascending)
    let bug = evo.population.members.select(1 + index)
    evo.population.members.pop(bug.key)
    result = bug.val

when false:
  proc remove*[T](rng: var Rand; population: var TreePop[T]; size: Positive;
                  count: Positive = 1) =
    var count = count
    while count > 0:
      if population.len < 1:
        raise ValueError.newException:
          "cannot run a tournament with empty population"
      let index = tournament(rng, population.high, size, order = Ascending)
      population.del(index)
      dec count

proc best*[T](population: TreeParsimony[T]): T =
  if population.members.len == 0:
    raise ValueError.newException "population is empty"
  else:
    let bug = population.members.max
    result = bug.val

proc best*[T](evo: TreeEvolver[T]): T =
  evo.population.best

proc randomMember*[T](evo: var TreeEvolver[T]): T =
  evo.population.select(1 + evo.rng.rand(evo.population.high))

proc run*[T](evo: var TreeEvolver[T]; op: GenomeOperatorSpec[T]): GenomeGroup[T] =
  var group: GenomeGroup[T]
  group.add evo.tournament()
  group.add evo.tournament()
  op.measureOperator:
    result = op.fn(evo.rng, group)

proc makeRoom*[T](evo: var TreeEvolver[T]; count = 1) =
  while evo.population.len > evo.tableau.maxPopulation - count:
    discard evo.evict()

proc push*[T](evo: var TreeEvolver[T]; s: Option[float]; item: T) =
  evo.population.push(s, item)

proc pop*[T](evo: var TreeEvolver[T]; s: Option[float]; item: T) =
  evo.population.pop(s, item)

proc add*[T](evo: var TreeEvolver[T]; item: sink T) =
  evo.makeRoom()
  evo.population.push(item)
