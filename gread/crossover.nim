import std/options
import std/random

import gread/ast
import gread/grammar
import gread/genotype

# FIXME: optimize this after confirming the semantics

type
  Invention[T] = tuple[ast: Ast[T]; genome: Genome]
  ResultForm[T] = Option[Invention[T]]

proc subtreeXoverImpl[T](rng: var Rand; gram: Grammar;
                         a, b: Genome): ResultForm[T] =
  # ensure the first crossover point occurs in mutual coding space
  let coding = min(a.high, b.high)
  let x = rng.rand(0..coding)
  let l = rng.rand(x..b.high) + 1
  # the second point can exceed the length of a shorter genome
  var g =
    if x > 0:
      a[0..<x] & b[x..<l]
    else:
      b[x..<l]
  if l <= a.high:
    g.add a[l..a.high]
  when greadWrapping:
    g.add g
    g.add g
    g.add g
  try:
    let bug = πFilling[T](gram, g)
    result = some bug
  except ShortGenome:
    result = none Invention[T]

iterator crossoverImpl[T](rng: var Rand; gram: Grammar;
                          a, b: Genome): ResultForm[T] =
  # ensure the crossover point occurs in mutual coding space
  let coding = min(a.high, b.high)
  if coding == 0:
    raise Defect.newException "attempt to crossover program lacking genome"
  let n = rng.rand(0..coding)
  for (x, y, i) in [(a, b, n), (b, a, coding-n)].items:
    var g = x[0..i]                                     # start with the head
    if i < y.high:                                      # there's a tail?
      g.add y[i..y.high]                                # add the tail
    when greadWrapping:
      g.add g                                           # double it
      g.add g                                           # again!
      g.add g                                           # again!
    try:
      let bug = πFilling[T](gram, g)
      yield some bug
    except ShortGenome:
      yield none Invention[T]

iterator randomSubtreeXover*[T](rng: var Rand; gram: Grammar;
                                a: Genome): ResultForm[T] =
  ## perform GE crossover between one parent and a random genome
  if a.len == 0:
    raise Defect.newException "received empty input genome"
  let b = randomGenome(rng, a.len)
  yield subtreeXoverImpl[T](rng, gram, a, b)

iterator subtreeXover*[T](rng: var Rand; gram: Grammar;
                          a, b: Genome): ResultForm[T] =
  ## perform GE crossover between two parents to form a new child
  if a.len == 0 or b.len == 0:
    raise Defect.newException "received empty input genome"
  yield subtreeXoverImpl[T](rng, gram, a, b)

iterator randomCrossover*[T](rng: var Rand; gram: Grammar;
                             a: Genome; size: int): ResultForm[T] =
  ## perform GE crossover between one parent and a random genome
  if a.len == 0:
    raise Defect.newException "received empty input genome"
  let b = randomGenome(rng, size)
  for r in crossoverImpl[T](rng, gram, a, b):
    yield r

iterator geCrossover*[T](rng: var Rand; gram: Grammar;
                         a, b: Genome): ResultForm[T] =
  ## perform GE crossover between two parents to form a new child
  if a.len == 0 or b.len == 0:
    raise Defect.newException "received empty input genome"
  for r in crossoverImpl[T](rng, gram, a, b):
    yield r
