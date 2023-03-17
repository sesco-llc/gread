import std/algorithm
import std/hashes
import std/math
import std/options
import std/random

import gread/aliasmethod
import gread/evolver
import gread/tableau
import gread/genotype

import trees/avl

import pkg/adix/stat except Option

type
  PopLike[T] = concept c
    c.push(T)
    c.pop() is T
    c.pop(T)
    c.min is T
    c.max is T
    c.len is Natural
    #c.high is Natural
    #c[Natural] is T
    c.select(Positive) is T
    c.rank(T) is Positive
    (T < T) is bool
    (T == T) is bool
    T.len is Natural
    T.score is Option[SomeFloat]
    for n in c.items:
      n is T

  Parsimony*[T] = object
    tree: AVLTree[Shim[T], T]
    parsimony: float
    lengths: MovingStat[float, uint32]
    scores: MovingStat[float, uint32]

  Shim[T] = object
    score: Option[float]
    data: Hash

proc downscale(s: Option[float]): float =
  result =
    if s.isSome:
      get s
    else:
      -Inf
  if result.isNaN:
    result = -Inf

proc `<`*[T](a, b: Shim[T]): bool =
  mixin `<`
  (downscale(a.score), a.data.hash) < (downscale(b.score), b.data.hash)

proc `==`*[T](a, b: Shim[T]): bool =
  a.data == b.data

proc covariance(parsimony: Parsimony): float =
  mixin score
  var n = 0
  for item in parsimony.tree.values:
    let s = item.score
    if s.isSome:
      inc n
      result += (get(s).float - parsimony.scores.mean) *
                (item.len.float - parsimony.lengths.mean)
  doAssert n == parsimony.lengths.n
  result =
    if n == 0:
      NaN
    else:
      result / n.float

proc sort[T](parsimony: var Parsimony[T]) =
  var stack = newSeqOfCap[T](parsimony.tree.len)
  while parsimony.tree.len > 0:
    let bug = parsimony.tree.max
    let item = parsimony.tree.pop(bug.key)
    stack.add item
  while stack.len > 0:
    let item = pop stack
    let shim = Shim[T](score: parsimony.score(item), data: item.hash)
    parsimony.tree.insert(shim, item)

proc recompute(parsimony: var Parsimony) =
  mixin score
  reset parsimony.lengths
  reset parsimony.scores
  if parsimony.tree.len == 0:
    parsimony.parsimony = NaN
  else:
    for item in parsimony.tree.values:
      let s = item.score
      if s.isSome:
        parsimony.scores.push get(s).float
        parsimony.lengths.push item.len.float
    parsimony.parsimony = parsimony.covariance / parsimony.lengths.variance
    sort parsimony

proc len*(parsimony: Parsimony): Natural =
  parsimony.tree.len

proc high*(parsimony: Parsimony): Natural =
  parsimony.tree.len.Natural - 1

proc min*[T](parsimony: Parsimony[T]): T =
  let bug = parsimony.tree.min
  result = bug.val.data

proc max*[T](parsimony: Parsimony[T]): T =
  let bug = parsimony.tree.max
  result = bug.val.data

proc `[]`*[T](parsimony: Parsimony[T]; index: Natural): T =
  let bug = parsimony.tree.select(1 + index)
  result = bug.val.data

proc score*[T](parsimony: Parsimony[T]; item: T): Option[float] =
  mixin score
  result = item.score
  if result.isSome:
    if not parsimony.parsimony.isNaN:
      # if parsimony is negative,
      #    reduce the score of longer programs,
      # else,
      #    raise the score of longer programs
      result = some: get(result) + parsimony.parsimony * item.len.float

proc select*[T](parsimony: Parsimony[T]; rank: Positive): T =
  let bug = parsimony.tree.select(rank)
  result = bug.val

proc rank*[T](parsimony: Parsimony[T]; item: T): Positive =
  let shim = Shim[T](score: parsimony.score(item), data: item.hash)
  result = parsimony.tree.rank(shim)

iterator items*[T](parsimony: Parsimony[T]): T =
  for item in parsimony.tree.values:
    yield item

proc push*[T](parsimony: var Parsimony[T]; item: T) =
  mixin score
  var shim = Shim[T](score: item.score, data: item.hash)
  if parsimony.tree.insert(shim, item):
    if item.score.isSome:
      recompute parsimony

proc pop*[T](parsimony: var Parsimony[T]; item: T) =
  let shim = Shim[T](score: parsimony.score(item), data: item.hash)
  if parsimony.tree.remove(shim):
    if shim.score.isSome:
      recompute parsimony

proc pop*[T](parsimony: var Parsimony[T]): T =
  let bug = parsimony.tree.min
  doAssert parsimony.tree.remove(bug.key)
  if bug.key.score.isSome:
    recompute parsimony
  result = bug.val

type
  TreeEvolver*[T] = object of GenomeEvolver[T]
    population*: Parsimony[T]

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
    let bug = evo.population.tree.select(1 + index)
    evo.population.tree.pop(bug.key)
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

proc best*[T](population: Parsimony[T]): T =
  if population.tree.len == 0:
    raise ValueError.newException "population is empty"
  else:
    let bug = population.tree.max
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
  var shim = Shim[T](score: s, data: item.hash)
  if evo.population.tree.insert(shim, item):
    if s.isSome:
      recompute evo.population

proc pop*[T](parsimony: var Parsimony[T]; s: Option[float]; item: T) =
  let shim = Shim[T](score: s, data: item.hash)
  if parsimony.tree.remove(shim):
    if shim.score.isSome:
      recompute parsimony

proc sort*[T](evo: var TreeEvolver[T]) =
  recompute evo.population
