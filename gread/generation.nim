import std/times
import std/options

import gread/tournament
import gread/population
import gread/spec
import gread/ast
import gread/programs
import gread/data
import gread/evolver

proc generation*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## try to create a novel program from better existing programs

  let clock = getTime()

  try:
    # inform the pop that we're in a new generation
    let gen = nextGeneration evo.population

    let operator = evo.randomOperator()
    profile "running the operator":
      result = operator evo

    if result.isSome:
      let p = get result
      # make room for the new program
      template size: int = evo.tableau.tournamentSize
      while evo.population.len > evo.tableau.maxPopulation-1:
        profile "loser's tournament":
          let loser = tournament(evo, size, order = Ascending)
          del(evo.population, loser.index)

      p.generation = gen
      p.core = evo.core
      # sample a single datapoint in order to check validity
      let s = evo.score(evo.randomSymbols, p)
      if s.isSome:
        p.score = get s
      else:
        p.zombie = true

      if not evo.tableau.requireValid or s.isSome:
        profile "add to pop":
          evo.population.add p

  finally:
    evo.generationTime (getTime() - clock).inMilliseconds.float
