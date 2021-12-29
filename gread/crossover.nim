import std/options
import std/strutils
import std/random

import gread/ast
import gread/grammar
import gread/genotype

# FIXME: optimize this after confirming the semantics

proc crossoverImpl[T](rng: var Rand; gram: Grammar;
                      a, b: Genome): Option[tuple[ast: Ast[T]; genome: Genome]] =
  # ensure the first crossover point occurs in mutual coding space
  let x = rng.rand(0..min(a.high, b.high))
  # the second point can exceed the length of the shorter genome
  let y = rng.rand((x+1)..b.len)
  var g = a[0..<x]
  g.add b[x..<y]
  if y < a.high:
    g.add a[y..a.high]
  let (pc, ast) = πGE[T](gram, g)                       # map the new genome
  result = some (ast: ast, genome: g[0..<pc.int])

proc randomCrossover*[T](rng: var Rand; gram: Grammar;
                         a: Genome): Option[tuple[ast: Ast[T]; genome: Genome]] =
  ## perform GE crossover between one parent and a random genome
  if a.len == 0:
    raise Defect.newException "received empty input genome"
  let b = randomGenome(rng, a.len)
  result = crossoverImpl(rng, gram, a, b)

proc geCrossover*[T](rng: var Rand; gram: Grammar;
                     a, b: Genome): Option[tuple[ast: Ast[T]; genome: Genome]] =
  ## perform GE crossover between two parents to form a new child
  if a.len == 0 or b.len == 0:
    raise Defect.newException "received empty input genome"
  result = crossoverImpl[T](rng, gram, a, b)
