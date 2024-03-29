import std/genasts
import std/macros
import std/options
import std/random
import std/strformat
import std/strutils

import gread/crossover
import gread/evolver
import gread/genotype
import gread/grammar
import gread/mutation
import gread/programs
import gread/tournament

proc geCrossover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover between two parents to form new children
  template size: Positive = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  let b = tournament(evo, size).program
  if a.genome.len == 0 or b.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in geCrossover[T](evo.rng, evo.grammar, a.genome, b.genome):
    evo.shortGenome(x.isNone)
    if x.isSome:
      result.add newProgram(x.get.ast, x.get.genome)

proc geMutation*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE mutation of a program to create novel offspring
  template size: Positive = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  var g = a.genome
  when greadWrapping:
    g.add g
    g.add g
    g.add g
  for x in geMutation[T](evo.rng, evo.grammar, g):
    evo.shortGenome(x.isNone)
    if x.isSome:
      result.add newProgram(x.get.ast, x.get.genome)

macro composeNoise(n: static string): untyped =
  let it = n.replace(".", "pt")
  var name = nnkAccQuoted.newTree(ident(fmt"geNoise{it}"))
  var iter = nnkAccQuoted.newTree(ident(fmt"geNoisy{it}"))
  name = postfix(name, "*")
  genAstOpt({}, name, iter, rs=ident"result"):
    proc name[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
      ## perform noisy GE mutation of a program to create novel offspring
      template size: Positive = evo.tableau.tournamentSize
      let a = tournament(evo, size).program
      var g = a.genome
      when greadWrapping:
        g.add g
        g.add g
        g.add g
      for x in iter[T](evo.rng, evo.grammar, g):
        evo.shortGenome(x.isNone)
        if x.isSome:
          rs.add newProgram(x.get.ast, x.get.genome)

    proc name[T](evo: var GenomeEvolver[T]): seq[T] =
      ## perform noisy GE mutation of a program to create novel offspring
      var g = evo.tournament()
      for genome in iter[T](evo.rng, g):
        rs.add genome

composeNoise("0.25")
composeNoise("0.5")
composeNoise("1.0")
composeNoise("2.0")
composeNoise("4.0")

proc asymmetricCrossover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover with a random genome to form new children
  template size: Positive = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  let b = tournament(evo, size).program
  if a.genome.len == 0 or b.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in asymmetricCrossover[T](evo.rng, evo.grammar, a.genome, b.genome):
    evo.shortGenome(x.isNone)
    if x.isSome:
      result.add newProgram(x.get.ast, x.get.genome)

proc randomCrossover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover with a random genome to form new children
  template size: Positive = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  if a.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in randomCrossover[T](evo.rng, evo.grammar, a.genome,
                              size = evo.tableau.seedProgramSize):
    evo.shortGenome(x.isNone)
    if x.isSome:
      result.add newProgram(x.get.ast, x.get.genome)

proc randomAsymmetricCrossover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover with a random genome to form new children
  template size: Positive = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  if a.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in randomAsymmetricCrossover[T](evo.rng, evo.grammar, a.genome,
                                        size = evo.tableau.seedProgramSize):
    evo.shortGenome(x.isNone)
    if x.isSome:
      result.add newProgram(x.get.ast, x.get.genome)

proc subtreeXover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover between two parents to form a new child
  template size: Positive = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  let b = tournament(evo, size).program
  if a.genome.len == 0 or b.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in subtreeXover[T](evo.rng, evo.grammar, a.genome, b.genome):
    evo.shortGenome(x.isNone)
    if x.isSome:
      result.add newProgram(x.get.ast, x.get.genome)

proc randomSubtreeXover*[T, V](evo: var Evolver[T, V]): seq[Program[T]] =
  ## perform GE crossover with a random genome to form a new child
  template size: Positive = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  if a.genome.len == 0:
    raise ValueError.newException:
      "population contains programs without genomes"
  for x in randomSubtreeXover[T](evo.rng, evo.grammar, a.genome):
    evo.shortGenome(x.isNone)
    if x.isSome:
      result.add newProgram(x.get.ast, x.get.genome)
