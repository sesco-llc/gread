import std/options

import gread/mutation
import gread/programs
import gread/tournament
import gread/crossover
import gread/evolver
import gread/genotype

proc geCrossover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover between two parents to form new children
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  let b = tournament(evo, size).program
  if a.genome.len == 0 or b.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in geCrossover[T](evo.rng, evo.grammar, a.genome, b.genome):
    if x.isSome:
      evo.shortGenome false
      result.add newProgram(x.get.ast, x.get.genome)
    else:
      evo.shortGenome true

proc randomCrossover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover with a random genome to form new children
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  if a.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in randomCrossover[T](evo.rng, evo.grammar, a.genome,
                              size = evo.tableau.seedProgramSize):
    if x.isSome:
      evo.shortGenome false
      result.add newProgram(x.get.ast, x.get.genome)
    else:
      evo.shortGenome true

proc subtreeXover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover between two parents to form a new child
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  let b = tournament(evo, size).program
  if a.genome.len == 0 or b.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in subtreeXover[T](evo.rng, evo.grammar, a.genome, b.genome):
    if x.isSome:
      evo.shortGenome false
      result.add newProgram(x.get.ast, x.get.genome)
    else:
      evo.shortGenome true

proc randomSubtreeXover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover with a random genome to form a new child
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  if a.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in randomSubtreeXover[T](evo.rng, evo.grammar, a.genome):
    if x.isSome:
      evo.shortGenome false
      result.add newProgram(x.get.ast, x.get.genome)
    else:
      evo.shortGenome true

proc geMutation*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE mutation of a program to create novel offspring
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  var g = a.genome
  when greadWrapping:
    g.add g
    g.add g
    g.add g
  for x in geMutation[T](evo.rng, evo.grammar, g):
    if x.isSome:
      evo.shortGenome false
      result.add newProgram(x.get.ast, x.get.genome)
    else:
      evo.shortGenome true
