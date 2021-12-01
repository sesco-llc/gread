import std/options
import std/strutils
import std/random

import gread/ast
import gread/grammar
import gread/genotype

proc subtreeCrossover*[T](a, b: Ast[T]): Ast[T] =
  audit a: echo a
  audit b: echo b
  if a.len == 0:
    raise Defect.newException "xover input is empty"
  else:
    var (x, y) = (rand(a.high), rand(b.high))

    # disabling this for now; i'd rather suffer the error
    #if not a.isFunctionSymbol(x) and not b.isFunctionSymbol(y):
    # FIXME: optimize this delete/insert to a replace operation
    let c = b.subtree(y)      # the subtree at index y
    audit c: echo "chunk in xover: ", c
    result = a.replace(x, c)   # replace node(s) at x with new subtree
  audit result: echo "xover result: ", result

proc geCrossover*[T](gram: Grammar[T];
                     a, b: Genome): Option[tuple[ast: Ast[T]; genome: Genome]] =
  ## perform GE crossover between two parents to form a new child
  if a.len == 0 or b.len == 0:
    raise ShortGenome.newException "received empty input genome"
  var g: Genome
  if a.len == 1:
    g = a & b
  elif b.len == 1:
    g = b & a
  else:

    # ensure the first crossover point occurs in mutual coding space
    let x = rand(0..min(a.high, b.high))
    # the second point can exceed the length of the shorter genome
    let y = rand(x..b.high)
    g = a[0..<x] & b[x..y]
  try:
    let (pc, ast) = gram.πGE(g)                       # map the new genome
    result = some: (ast: ast, genome: g[0..<pc.int])  # throw away the tail
  except ShortGenome:
    discard
