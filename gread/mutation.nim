import std/options
import std/random

import gread/ast
import gread/grammar
import gread/genotype

type
  Invention[T] = tuple[ast: Ast[T]; genome: Genome]
  ResultForm[T] = Option[Invention[T]]

iterator geMutation*[T](rng: var Rand; gram: Grammar;
                        a: Genome): ResultForm[T] =
  ## perform GE mutation of a program to create novel offspring
  if a.len == 0:
    raise Defect.newException "received empty input genome"
  var g = a
  g.string[rng.rand(g.high)] = rng.rand(int char.high).char
  try:
    let (pc {.used.}, ast) = πGE[T](gram, g)                    # map the new genome
    yield some (ast: ast, genome: g)
  except ShortGenome:
    yield none Invention[T]
