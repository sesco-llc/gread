import std/options

import gread/tournament
import gread/population
import gread/spec
import gread/ast
import gread/programs
import gread/manager

proc generation*[T](man: var Manager[T]): Option[Program[T]] =
  ## try to make something amazing
  if man.fitness.isNil:
    raise Defect.newException "assign a fitness function first"

  # inform the pop that we're in a new generation
  let gen = nextGeneration man.population

  let operator = man.randomOperator()
  profile "operator":
    result = operator man

  if result.isSome:
    let p = get result
    # make room for the new program
    template size: int = man.tableau.tournamentSize
    while man.population.len > man.tableau.maxPopulation-1:
      profile "loser's tournament":
        let loser = tournament(man, size, order = Ascending)
        del(man.population, loser.index)

    p.generation = gen
    profile "new score":
      p.score = man.score(p)
    if p.isValid:
      # we only add valid programs to the population
      man.population.add p
    else:
      p.zombie = true
