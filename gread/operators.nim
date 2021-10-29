import std/options

import gread/mutation
import gread/programs
import gread/tournament
import gread/fertilizer
import gread/crossover
import gread/evolver

proc pointPromotion*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  result = some: newProgram pointPromotion(evo.primitives, a.ast)

proc pointMutation*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  result = some: newProgram pointMutation(evo.primitives, a.ast)

proc removeOneLeaf*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  result = some: newProgram removeOneLeaf(evo.primitives, a.ast,
                                          size = evo.tableau.seedProgramSize)

proc randomCrossover*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  let b = randProgram(evo.primitives, size = evo.tableau.seedProgramSize)
  result = some: newProgram subtreeCrossover(a.ast, b.ast)

proc subtreeCrossover*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  template size: int = evo.tableau.tournamentSize
  let a = tournament(evo, size).program
  let b = tournament(evo, size).program
  result = some: newProgram subtreeCrossover(a.ast, b.ast)
