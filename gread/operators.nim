import std/options

import gread/population
import gread/mutation
import gread/programs
import gread/tournament
import gread/fertilizer
import gread/crossover
import gread/manager

proc pointPromotion*[T](man: var Manager[T]): Option[Program[T]] =
  template size: int = man.tableau.tournamentSize
  let a = tournament(man, size).program
  result = some: newProgram pointPromotion(man.primitives, a.ast)

proc pointMutation*[T](man: var Manager[T]): Option[Program[T]] =
  template size: int = man.tableau.tournamentSize
  let a = tournament(man, size).program
  result = some: newProgram pointMutation(man.primitives, a.ast)

proc addOrRemoveLeaves*[T](man: var Manager[T]): Option[Program[T]] =
  template size: int = man.tableau.tournamentSize
  let a = tournament(man, size).program
  result = some: newProgram addOrRemoveLeaves(man.primitives, a.ast,
                                              size = man.tableau.seedProgramSize)

proc randomCrossover*[T](man: var Manager[T]): Option[Program[T]] =
  template size: int = man.tableau.tournamentSize
  let a = tournament(man, size).program
  let b = randProgram(man.primitives, size = man.tableau.seedProgramSize)
  result = some: newProgram subtreeCrossover(a.ast, b.ast)

proc subtreeCrossover*[T](man: var Manager[T]): Option[Program[T]] =
  template size: int = man.tableau.tournamentSize
  let a = tournament(man, size).program
  let b = tournament(man, size).program
  result = some: newProgram subtreeCrossover(a.ast, b.ast)
