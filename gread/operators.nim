import std/options

import gread/mutation
import gread/programs
import gread/tournament
import gread/fertilizer
import gread/crossover
import gread/evolver
import gread/grammar
import gread/genotype

proc geCrossover*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## perform GE crossover between two parents to form a new child
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  let b = tournament(evo, size).program
  if a.genome.len == 0 or b.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  try:
    let x = geCrossover[T](evo.grammar, a.genome, b.genome)
    if x.isSome:
      result = some: newProgram(x.get.ast, x.get.genome)
    evo.shortGenome false
  except ShortGenome:
    evo.shortGenome true

proc randomCrossover*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## perform GE crossover with a random genome to form a new child
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  if a.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  try:
    let b = randomGenome(evo.tableau.seedProgramSize)
    let x = geCrossover[T](evo.grammar, a.genome, b)
    if x.isSome:
      result = some: newProgram(x.get.ast, x.get.genome)
    evo.shortGenome false
  except ShortGenome:
    evo.shortGenome true

proc geMutation*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## perform GE mutation of a program to create novel offspring
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  try:
    let x = geMutation[T](evo.grammar, a.genome)
    if x.isSome:
      result = some: newProgram(x.get.ast, x.get.genome)
    evo.shortGenome false
  except ShortGenome:
    evo.shortGenome true
