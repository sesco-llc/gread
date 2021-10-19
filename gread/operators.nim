import std/options

import gread/population
import gread/mutation
import gread/programs
import gread/tournament
import gread/fertilizer
import gread/crossover

proc pointPromotion*[T](pop: var Population[T]): Option[Program[T]] =
  template size: int = pop.tableau.tournamentSize
  let a = tournament(pop, size).program
  result = some: newProgram pointPromotion(a.ast)

proc pointMutation*[T](pop: var Population[T]): Option[Program[T]] =
  template size: int = pop.tableau.tournamentSize
  let a = tournament(pop, size).program
  result = some: newProgram pointMutation(pop.primitives, a.ast)

proc addOrRemoveLeaves*[T](pop: var Population[T]): Option[Program[T]] =
  template size: int = pop.tableau.tournamentSize
  let a = tournament(pop, size).program
  result = some: newProgram addOrRemoveLeaves(pop.primitives, a.ast,
                                              size = pop.tableau.seedProgramSize)

proc randomCrossover*[T](pop: var Population[T]): Option[Program[T]] =
  template size: int = pop.tableau.tournamentSize
  let a = tournament(pop, size).program
  let b = randProgram(pop.primitives, size = pop.tableau.seedProgramSize)
  result = some: newProgram subtreeCrossover(a.ast, b.ast)

proc subtreeCrossover*[T](pop: var Population[T]): Option[Program[T]] =
  template size: int = pop.tableau.tournamentSize
  let a = tournament(pop, size).program
  let b = tournament(pop, size).program
  result = some: newProgram subtreeCrossover(a.ast, b.ast)
