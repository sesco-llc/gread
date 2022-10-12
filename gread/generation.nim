import std/times
import std/options

import gread/tournament
import gread/population
import gread/spec
import gread/ast
import gread/programs
import gread/data
import gread/evolver
import gread/tableau

proc makeRoom*[T, V](evo: var Evolver[T, V]; space = 1) =
  ## create some space in the population whatfer adding a new program
  template size: int = evo.tableau.tournamentSize
  while evo.population.len > evo.tableau.maxPopulation-space:
    let loser = tournament(evo, size, order = Ascending)
    del(evo, loser.program)            # warn evolver to clear cache
    del(evo.population, loser.index)   # rm program from population

iterator generation*[T, V](evo: var Evolver[T, V]): Program[T] =
  ## try to create novel program(s) from better existing programs

  # inform the pop that we're in a new generation
  let gen = nextGeneration evo
  let clock = getTime()
  var discoveries = 0

  try:
    while discoveries == 0:
      let operator = evo.randomOperator()
      for p in operator(evo).items:
        p.generation = Generation gen
        p.core = evo.core
        # FIXME: optimization point
        let s =
          if evo.isEqualWeight:
            # sample a single datapoint in order to check validity
            evo.scoreRandomly(p)
          else:
            # sample all datapoints in order to develop useful score
            evo.score(p)
        if s.isSome:
          p.score = strength(evo)(get s)
          evo.maybeResetFittest(p)
        else:
          p.zombie = true

        if RequireValid notin evo.tableau or p.isValid:
          inc discoveries   # we have something worth adding
          evo.makeRoom()
          evo.population.add p
          yield p

  finally:
    if discoveries > 0:
      # update the parsimony to account for additions and removals
      discard evo.resetParsimony()

    evo.generationTime (getTime() - clock).inMilliseconds.float
