import std/random
import std/sequtils

import pkg/balls

import gread/ast
import gread/programs
import gread/grammar
import gread/fertilizer
import gread/maths

import ./glang

suite "trees":
  var trees: seq[Program[G]]
  block:
    ## tree tests
    var gram: Grammar
    initGrammar[G](gram, glangGrammar)
    while trees.len < 10_000:
      try:
        trees.add:
          randProgram[G](randState(), gram, size=100)  #
      except ShortGenome:
        discard
    let sizes = mapIt(trees, it.len)
    checkpoint "average size: ", avg(sizes)

  block:
    ## dig them spinners
    for i, p in trees.pairs:
      checkpoint $p
      if i == 10:
        break
