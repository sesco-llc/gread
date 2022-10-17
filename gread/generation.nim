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
        p.generation = gen
        p.core = evo.core
        if RequireValid in evo.tableau:
          let s = evo.score(p)
          if s.isSome:
            p.score = strength(evo)(get s)
            if p.isValid:
              makeRoom evo
              evo.add p
              inc discoveries   # we have something worth adding
              evo.discover(p)
              yield p
          else:
            p.zombie = true
        else:
          makeRoom evo
          evo.add p
          inc discoveries   # we have something worth adding
          evo.discover(p)
          yield p

  finally:
    if discoveries > 0:
      # update the parsimony to account for additions and removals
      discard evo.resetParsimony()

    evo.generationTime (getTime() - clock).inMilliseconds.float
