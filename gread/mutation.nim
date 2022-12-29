import std/genasts
import std/macros
import std/options
import std/random
import std/strformat
import std/strutils

import gread/ast
import gread/grammar
import gread/genotype

type
  Invention[T] = tuple[ast: Ast[T]; genome: Genome]
  ResultForm[T] = Option[Invention[T]]

iterator geMutation*[T](rng: var Rand; a: T): T =
  ## perform GE mutation of a genome to create novel offspring
  if a.len == 0:
    raise Defect.newException "received empty input genome"
  var g = a
  g[rng.rand(g.high)] = rng.rand(int char.high).char
  yield g

iterator geMutation*[T](rng: var Rand; gram: Grammar;
                        a: Genome): ResultForm[T] =
  ## perform GE mutation of a program to create novel offspring
  for genome in geMutation(rng, a):
    try:
      yield some πFilling[T](gram, genome)
    except ShortGenome:
      yield none Invention[T]

macro composeNoise(n: static string): untyped =
  let it = n.replace(".", "pt")
  var iter = nnkAccQuoted.newTree(ident(fmt"geNoisy{it}"))
  iter = postfix(iter, "*")
  let n = parseFloat(n) * 0.01
  let assign = bindSym "[]="
  genAstOpt({}, n, assign, name = iter):
    iterator name[T](rng: var Rand; gram: Grammar;
                     a: Genome): ResultForm[T] =
      ## apply noise
      if a.len == 0:
        raise Defect.newException "received empty input genome"
      var g = a
      for i in g.low..g.high:
        if rng.rand(1.0) < n:
          # g[i] = rng.rand(int char.high).char
          assign(g, i, rng.rand(int char.high).char)
      try:
        yield some πFilling[T](gram, g)
      except ShortGenome:
        yield none Invention[T]

    iterator name[T](rng: var Rand; a: T): T =
      ## apply noise
      if a.len == 0:
        raise Defect.newException "received empty input genome"
      var g = a
      for i in g.low..g.high:
        if rng.rand(1.0) < n:
          # g[i] = rng.rand(int char.high).char
          assign(g, i, rng.rand(int char.high).char)
      yield g

composeNoise("0.25")
composeNoise("0.5")
composeNoise("1.0")
composeNoise("2.0")
composeNoise("4.0")
