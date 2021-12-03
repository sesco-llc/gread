import std/options
import std/strutils
import std/random

import gread/ast
import gread/grammar
import gread/genotype

proc geCrossover*[T](gram: Grammar[T];
                     a, b: Genome): Option[tuple[ast: Ast[T]; genome: Genome]] =
  ## perform GE crossover between two parents to form a new child
  if a.len == 0 or b.len == 0:
    raise ShortGenome.newException "received empty input genome"
  # ensure the first crossover point occurs in mutual coding space
  let x = rand(0..min(a.high, b.high))
  # the second point can exceed the length of the shorter genome
  let y = rand((x+1)..b.len)
  var g = a[0..<x]
  g.add b[x..<y]
  if y < a.high:
    g.add a[y..a.high]
  try:
    let (pc, ast) = gram.πGE(g)                       # map the new genome
    result = some (ast: ast, genome: g[0..<pc.int])
  except ShortGenome:
    #echo "short crossover; ", a.len, " and ", b.len, " and ", g.len
    raise
