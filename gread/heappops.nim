import std/algorithm
import std/random

import gread/aliasmethod
import gread/evolver
import gread/tableau
import gread/genotype


type
  HeapPop*[T] = object
    data: seq[T]
    cmp*: proc(a, b: T): bool

proc initHeapPop*[T](cmp: proc(a, b: T): bool; initialSize: Natural = 4): HeapPop[T] =
  result.cmp = cmp
  result.data = newSeqOfCap[T](initialSize)

proc initHeapPop*[T](initialSize: Natural = 4): HeapPop[T] =
  mixin `<`
  initHeapPop[T](`<`, initialSize = initialSize)

proc len*[T](heap: HeapPop[T]): int {.inline.} =
  ## Returns the number of elements of `heap`.
  runnableExamples:
    let heap = [9, 5, 8].toHeapPop
    assert heap.len == 3

  heap.data.len

proc `[]`*[T](heap: HeapPop[T], i: Natural): lent T {.inline.} =
  ## Accesses the i-th element of `heap`.
  heap.data[i]

proc siftup[T](heap: var HeapPop[T], startpos, p: int) =
  ## `heap` is a heap at all indices >= `startpos`, except possibly for `p`. `p`
  ## is the index of a leaf with a possibly out-of-order value. Restores the
  ## heap invariant.
  var pos = p
  let newitem = heap[pos]
  # Follow the path to the root, moving parents down until finding a place
  # newitem fits.
  while pos > startpos:
    let parentpos = (pos - 1) shr 1
    let parent = heap[parentpos]
    if heap.cmp(newitem, parent):
      heap.data[pos] = parent
      pos = parentpos
    else:
      break
  heap.data[pos] = newitem

proc siftdownToBottom[T](heap: var HeapPop[T], p: int) =
  # This is faster when the element should be close to the bottom.
  let endpos = len(heap)
  var pos = p
  let startpos = pos
  let newitem = heap[pos]
  # Bubble up the smaller child until hitting a leaf.
  var childpos = 2 * pos + 1 # leftmost child position
  while childpos < endpos:
    # Set childpos to index of smaller child.
    let rightpos = childpos + 1
    if rightpos < endpos and not heap.cmp(heap[childpos], heap[rightpos]):
      childpos = rightpos
    # Move the smaller child up.
    heap.data[pos] = heap[childpos]
    pos = childpos
    childpos = 2 * pos + 1
  # The leaf at pos is empty now. Put newitem there, and bubble it up
  # to its final resting place (by sifting its parents down).
  heap.data[pos] = newitem
  siftup(heap, startpos, pos)

proc siftdown[T](heap: var HeapPop[T], p: int) =
  let endpos = len(heap)
  var pos = p
  let newitem = heap[pos]
  var childpos = 2 * pos + 1
  while childpos < endpos:
    let rightpos = childpos + 1
    if rightpos < endpos and not heap.cmp(heap[childpos], heap[rightpos]):
      childpos = rightpos
    if not heap.cmp(heap[childpos], newitem):
      break
    heap.data[pos] = heap[childpos]
    pos = childpos
    childpos = 2 * pos + 1
  heap.data[pos] = newitem

proc push*[T](heap: var HeapPop[T], item: sink T) =
  ## Pushes `item` onto `heap`, maintaining the heap invariant.
  heap.data.add(item)
  siftup(heap, 0, len(heap) - 1)

proc pop*[T](heap: var HeapPop[T]): T =
  ## Pops and returns the smallest item from `heap`,
  ## maintaining the heap invariant.
  runnableExamples:
    var heap = [9, 5, 8].toHeapPop
    assert heap.pop() == 5

  let lastelt = heap.data.pop()
  if heap.len > 0:
    result = heap[0]
    heap.data[0] = lastelt
    siftdownToBottom(heap, 0)
  else:
    result = lastelt

proc find*[T](heap: HeapPop[T], x: T): int =
  ## Linear scan to find the index of the item `x` or -1 if not found.
  runnableExamples:
    let heap = [9, 5, 8].toHeapPop
    assert heap.find(5) == 0
    assert heap.find(9) == 1
    assert heap.find(777) == -1

  result = -1
  for i in 0 ..< heap.len:
    if heap[i] == x: return i

proc del*[T](heap: var HeapPop[T], index: Natural) =
  ## Removes the element at `index` from `heap`, maintaining the heap invariant.
  runnableExamples:
    var heap = [9, 5, 8].toHeapPop
    heap.del(1)
    assert heap[0] == 5
    assert heap[1] == 8

  swap(heap.data[^1], heap.data[index])
  let newLen = heap.len - 1
  heap.data.setLen(newLen)
  if index < newLen:
    siftdownToBottom(heap, index)

proc replace*[T](heap: var HeapPop[T], item: sink T): T =
  ## Pops and returns the current smallest value, and add the new item.
  ## This is more efficient than `pop()` followed by `push()`, and can be
  ## more appropriate when using a fixed-size heap. Note that the value
  ## returned may be larger than `item`! That constrains reasonable uses of
  ## this routine unless written as part of a conditional replacement.
  ##
  ## **See also:**
  ## * `pushpop proc <#pushpop,HeapPop[T],sinkT>`_
  runnableExamples:
    var heap = [5, 12].toHeapPop
    assert heap.replace(6) == 5
    assert heap.len == 2
    assert heap[0] == 6
    assert heap.replace(4) == 6

  result = heap[0]
  heap.data[0] = item
  siftdown(heap, 0)

proc pushpop*[T](heap: var HeapPop[T], item: sink T): T =
  ## Fast version of a `push()` followed by a `pop()`.
  ##
  ## **See also:**
  ## * `replace proc <#replace,HeapPop[T],sinkT>`_
  runnableExamples:
    var heap = [5, 12].toHeapPop
    assert heap.pushpop(6) == 5
    assert heap.len == 2
    assert heap[0] == 6
    assert heap.pushpop(4) == 4

  result = item
  if heap.len > 0 and heap.cmp(heap.data[0], result):
    swap(result, heap.data[0])
    siftdown(heap, 0)

proc clear*[T](heap: var HeapPop[T]) =
  ## Removes all elements from `heap`, making it empty.
  runnableExamples:
    var heap = [9, 5, 8].toHeapPop
    heap.clear()
    assert heap.len == 0

  heap.data.setLen(0)

proc `$`*[T](heap: HeapPop[T]): string =
  ## Turns a heap into its string representation.
  runnableExamples:
    let heap = [1, 2].toHeapPop
    assert $heap == "[1, 2]"

  result = "["
  for x in heap.data:
    if result.len > 1: result.add(", ")
    result.addQuoted(x)
  result.add("]")

proc high[T](heap: HeapPop[T]): int {.inline.} =
  heap.len - 1

proc capacity*[T](heap: HeapPop[T]): int {.inline.} =
  heap.data.capacity

iterator items*[T](q: HeapPop[T]): T =
  ## helper for heappop-based populations
  for i in 0..<q.len:
    yield q[i]

type
  HeapOperator*[T] = proc(evo: var HeapEvolver[T]): seq[T] {.nimcall.}

  HeapEvolver*[T] = object of GenomeEvolver[T]
    population*: HeapPop[T]

proc initEvolver*[T](evo: var HeapEvolver[T]; tableau: Tableau; rng: Rand = randState()) =
  ## perform initial setup of the Evolver, binding tableau
  initEvolver(evo.LeanEvolver, tableau, rng)

proc tournament*[T](evo: var HeapEvolver[T]; size: Positive;
                    order = Descending): T =
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"
  let order =
    # swap the order because this is a max heap
    case order
    of Descending: Ascending
    of Ascending: Descending
  let index = tournament(evo.rng, evo.population.high, size, order = order)
  result = evo.population[index]

proc tournament*[T](evo: var HeapEvolver[T]; order = Descending): T =
  tournament(evo, evo.tableau.tournamentSize, order = order)

proc evict*[T](rng: var Rand; population: var HeapPop[T]; size: Positive): T =
  if population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"
  let index = tournament(rng, population.high, size, order = Descending)
  result = population[index]
  population.del(index)

proc remove*[T](rng: var Rand; population: var HeapPop[T]; size: Positive;
                count: Positive = 1) =
  var count = count
  while count > 0:
    if population.len < 1:
      raise ValueError.newException:
        "cannot run a tournament with empty population"
    let index = tournament(rng, population.high, size, order = Descending)
    population.del(index)
    dec count

proc best*[T](population: HeapPop[T]): T =
  if population.len == 0:
    raise ValueError.newException "population is empty"
  else:
    result = population[0]

proc randomMember*[T](evo: var HeapEvolver[T]): T =
  evo.population[evo.rng.rand(evo.population.high)]

proc sort*[T](population: var HeapPop[T]) =
  let better = population.cmp
  proc compare(a, b: T): int =
    if better(a, b):
      -1
    else:
      1

  sort(population.data, compare, Descending)

proc run*[T](evo: var HeapEvolver[T]; op: GenomeOperatorSpec[T]): GenomeGroup[T] =
  var group: GenomeGroup[T]
  group.add evo.tournament()
  group.add evo.tournament()
  op.measureOperator:
    result = op.fn(evo.rng, group)

proc makeRoom*[T](evo: var HeapEvolver[T]; count = 1) =
  while evo.population.len > evo.tableau.maxPopulation - count:
    discard evo.evict(evo.rng, evo.population, evo.tableau.tournamentSize)
