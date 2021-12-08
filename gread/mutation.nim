import std/options
import std/sequtils
import std/random

import gread/spec
import gread/ast
import gread/grammar
import gread/genotype

proc geMutation*[T](gram: Grammar[T];
                    a: Genome): Option[tuple[ast: Ast[T]; genome: Genome]] =
  ## perform GE mutation of a program to create novel offspring
  if a.len == 0:
    raise Defect.newException "received empty input genome"
  var g = a
  g.string[rand(g.high)] = rand(int char.high).char
  let (pc, ast) = gram.πGE(g)                       # map the new genome
  result = some (ast: ast, genome: g)
