import std/times
import std/options

import gread/tournament
import gread/population
import gread/spec
import gread/ast
import gread/programs
import gread/data
import gread/evolver

iterator generation*[T, V](evo: var Evolver[T, V]): Program[T] =
  ## try to create novel program(s) from better existing programs
  mixin strength

  # inform the pop that we're in a new generation
  let gen = nextGeneration evo.population
  let clock = getTime()
  var discoveries = 0

  try:
    while discoveries == 0:
      let operator = evo.randomOperator()
      for p in operator(evo).items:
        p.generation = gen
        p.core = evo.core
        # FIXME: optimization point
        let s =
          when defined(greadFast):
            # sample a single datapoint in order to check validity
            evo.scoreRandomly(p)
          else:
            # sample all datapoints in order to develop useful score
            evo.score(p)
        if s.isSome:
          p.score = strength(get s)
        else:
          p.zombie = true

        if not evo.tableau.requireValid or p.isValid:
          inc discoveries   # we have something worth adding
          # make room for the new program
          template size: int = evo.tableau.tournamentSize
          while evo.population.len > evo.tableau.maxPopulation-1:
            profile "loser's tournament":
              let loser = tournament(evo, size, order = Ascending)
              del(evo, loser.program)            # warn evolver to clear cache
              del(evo.population, loser.index)   # rm program from population
          evo.population.add p
          yield p

  finally:
    if discoveries > 0:
      # update the parsimony to account for additions and removals
      resetParsimony evo.population

    evo.generationTime (getTime() - clock).inMilliseconds.float
