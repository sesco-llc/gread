import std/options

import gread/tournament
import gread/population
import gread/spec
import gread/ast
import gread/programs

proc generation*[T](pop: var Population[T]): Program[T] =
  ## try to make something amazing
  if pop.fitness.isNil:
    raise Defect.newException "assign a fitness function first"

  # inform the pop that we're in a new generation
  let generation = nextGeneration pop

  let fun = randomOperator pop
  profile "operator":
    let evo = fun pop
  if evo.isNone:
    result = nil
  else:
    # make room for the new program
    template size: int = pop.tableau.tournamentSize
    while pop.len > pop.tableau.maxPopulation-1:
      profile "loser's tournament":
        let loser = tournament(pop, size, order = Ascending)
        del(pop, loser.index)

    result = get evo
    result.generation = generation
    profile "new score":
      result.score = pop.score(result)
    if result.score.isValid:
      # we only add valid programs to the population
      pop.add result
    else:
      result.zombie = true
