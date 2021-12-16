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
        p.score = get s
      else:
        p.zombie = true

      if not evo.tableau.requireValid or p.isValid:
        # make room for the new program
        template size: int = evo.tableau.tournamentSize
        while evo.population.len > evo.tableau.maxPopulation-1:
          profile "loser's tournament":
            let loser = tournament(evo, size, order = Ascending)
            del(evo, loser.program)            # warn evolver to clear cache
            del(evo.population, loser.index)   # rm program from population

        # we cannot afford to do so much work just for reporting
        when false and defined(greadFast):
          if not evo.population.fittest.isNil:
            if not evo.population.fittest.hash == p.hash:
              var samples = evo.randomDataIndexes()
              discard confidentComparison(evo, samples, p,
                                          evo.population.fittest)
              let s = evo.scoreFromCache(p)
              if s.isSome:
                p.score = get s
              else:
                p.zombie = true

        if not evo.tableau.requireValid or p.isValid:
          evo.population.add p

        # update the parsimony to account for additions and removals
        resetParsimony evo.population

  finally:
    evo.generationTime (getTime() - clock).inMilliseconds.float
