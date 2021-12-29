import std/random

import gread/spec
import gread/ast
import gread/programs
import gread/genotype
import gread/grammar

proc randProgram*[T](r: var Rand; gram: Grammar; size = 20): Program[T] =
  ## using the provided rng state, produce a random
  ## program of (up to) the given size
  let genome = randomGenome(r, size)
  let (pc, ast) = πGE[T](gram, genome)
  result = newProgram(ast, genome[0..<pc.int])
