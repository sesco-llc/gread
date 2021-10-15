import std/random

import gread/tournament
import gread/population
import gread/crossover
import gread/mutation
import gread/spec
import gread/ast
import gread/fertilizer

proc generation*[T](pop: var Population[T]): Program[T] =
  ## try to make something amazing
  if pop.fitness.isNil:
    raise Defect.newException "assign a fitness function first"

  # inform the pop that we're in a new generation
  let generation = nextGeneration pop

  template size: int = pop.tableau.tournamentSize

  # make room for the new program
  while pop.len > pop.tableau.maxPopulation-1:
    let loser = tournament(pop, size, order = Ascending)
    del(pop, loser.index)

  let a = tournament(pop, size).program
  let choice = rand 1.0
  let ast =
    if choice < 0.01:
      let b = randProgram(pop.primitives, size = pop.tableau.seedProgramSize)
      subtreeCrossover(a.ast, b.ast)
    elif choice < 0.05:
      addOrRemoveLeaves(pop.primitives, a.ast, size = pop.tableau.seedProgramSize)
    elif choice < 0.07:
      pointPromotion(a.ast)
    elif choice < 0.10:
      pointMutation(pop.primitives, a.ast)
    else:
      let b = tournament(pop, size).program
      subtreeCrossover(a.ast, b.ast)

  auditLength ast
  result = newProgram ast
  result.generation = generation
  result.score = pop.score(result)
  if not result.score.isValid:
    result.zombie = true
  pop.add result
