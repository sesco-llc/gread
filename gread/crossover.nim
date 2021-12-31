import std/options
import std/strutils
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
  let x = rng.rand(0..min(a.high, b.high))
  # the second point can exceed the length of the shorter genome
  let y = rng.rand((x+1)..b.len)
  var g = a[0..<x]
  g.add b[x..<y]
  if y < a.high:
    g.add a[y..a.high]
  try:
    let (pc, ast) = πGE[T](gram, g)                     # map the new genome
    result = some (ast: ast, genome: g[0..<pc.int])
  except ShortGenome:
    result = none Invention[T]

iterator crossoverImpl[T](rng: var Rand; gram: Grammar;
                          a, b: Genome): ResultForm[T] =
  # ensure the crossover point occurs in mutual coding space
  let n = rng.rand(0..min(a.high, b.high))
  for (x, y) in [(a, b), (b, a)].items:
    var g = x[0..<n]                                    # start with the head
    g.add y[n..<y.high]                                 # add an opposite tail
    try:
      let (pc, ast) = πGE[T](gram, g)                   # map the new genome
      yield some (ast: ast, genome: g[0..<pc.int])
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
                             a: Genome): ResultForm[T] =
  ## perform GE crossover between one parent and a random genome
  if a.len == 0:
    raise Defect.newException "received empty input genome"
  let b = randomGenome(rng, a.len)
  for r in crossoverImpl[T](rng, gram, a, b):
    yield r

iterator geCrossover*[T](rng: var Rand; gram: Grammar;
                         a, b: Genome): ResultForm[T] =
  ## perform GE crossover between two parents to form a new child
  if a.len == 0 or b.len == 0:
    raise Defect.newException "received empty input genome"
  for r in crossoverImpl[T](rng, gram, a, b):
    yield r
