import std/random
import std/deques

##[

This is an implementation of the Vose alias method as described in
https://en.wikipedia.org/wiki/Alias_method

The structure provides random weighted selections cheaply.

We give it a series of `(item: T, weight: float)` and rescales these and
pre-computes a pair of lists that let us trade initialization time/space
for subsequent query speed.

]##

type
  AliasMethod*[T] = object
    prob: seq[float64]
    alias: seq[int]
    data: seq[T]

proc initAliasMethod*[T](am: var AliasMethod;
                         input: openArray[(T, float or float64)]) =
  ## initialize the collection of items with given weights
  var n = input.len
  setLen(am.prob, n)
  setLen(am.alias, n)
  setLen(am.data, n)

  # rescale the probabilities according to the quantity
  var rescaled = newSeqOfCap[tuple[value: T, weight: float64]](n)
  for (op, weight) in input.items:
    rescaled.add (op, float64(weight) * n.float64)

  # sort the ops into deques of indices, according probability
  var small, large: Deque[int]
  for i, pair in rescaled.pairs:
    am.data[i] = pair.value
    if pair.weight < 1.0:
      small.addLast i
    else:
      large.addLast i

  # consume and accumulate weights
  while small.len > 0 and large.len > 0:
    # consume
    let (l, g) = (popFirst small, popFirst large)
    am.prob[l] = rescaled[l].weight
    am.alias[l] = g
    # accumulate remainder
    rescaled[g].weight += rescaled[l].weight-1.0
    if rescaled[g].weight < 1.0:
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

proc choose*[T](am: AliasMethod[T]): T =
  ## weighted random choice from the list of inputs
  let i = rand(am.prob.high)
  `[]`(am.data):
    if rand(1.0) < am.prob[i]:
      i
    else:
      am.alias[i]

proc len*(am: AliasMethod): int =
  ## the cardinality of values
  am.data.len
