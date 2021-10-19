import std/options
import std/random

import gread/tournament
import gread/population
import gread/crossover
import gread/mutation
import gread/spec
import gread/ast
import gread/fertilizer
import gread/programs

proc generation*[T](pop: var Population[T]): Program[T] =
  ## try to make something amazing
  if pop.fitness.isNil:
    raise Defect.newException "assign a fitness function first"

  # inform the pop that we're in a new generation
  let generation = nextGeneration pop

  let fun = randomOperator pop
  let evo = fun pop
  if evo.isNone:
    result = nil
  else:
    # make room for the new program
    template size: int = pop.tableau.tournamentSize
    while pop.len > pop.tableau.maxPopulation-1:
      let loser = tournament(pop, size, order = Ascending)
      del(pop, loser.index)

    result = get evo
    result.generation = generation
    result.score = pop.score(result)
    if not result.score.isValid:
      result.zombie = true
    pop.add result
