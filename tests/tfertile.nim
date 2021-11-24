import std/sequtils

import pkg/balls

import gread/ast
import gread/programs
import gread/primitives
import gread/fertilizer
import gread/maths

import ./glang

suite "trees":
  var prims = newPrimitives[G]()
  var trees: seq[Program[G]]
  block:
    ## tree tests
    prims.constants = @[term 1, term 2, term 3, sym"a", sym"b", sym"c"]
    prims.functions = @[fun"+", fun"-", fun"/", fun"*"]
    while trees.len < 10_000:
      trees.add:
        randProgram(prims, size=10)  #
    let sizes = mapIt(trees, it.len)
    checkpoint "average size: ", avg(sizes)

  block:
    ## dig them spinners
    for i, p in trees.pairs:
      checkpoint render(prims, p.ast)
      if i == 10:
        break
