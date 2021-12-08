import std/random

import gread/spec
import gread/ast
import gread/programs
import gread/genotype
import gread/grammar

proc randProgram*[T](gram: Grammar[T]; size = 20): Program[T] =
  ## produce a random program of, roughly, the given size
  let genome = randomGenome size
  let (pc, ast) = gram.πGE(genome)
  result = newProgram(ast, genome[0..<pc.int])
