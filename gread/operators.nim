import std/options

import gread/mutation
import gread/programs
import gread/tournament
import gread/fertilizer
import gread/crossover
import gread/evolver
import gread/primitives

proc pointPromotion*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## promote a random child to its parent's place in the tree, discarding
  ## both the parent and any peers of the child
  template size: int = evo.tableau.tournamentSize
  template c: Primitives[T] = evo.primitives
  let a = tournament(evo, size).program
  result = some: c.newProgram pointPromotion(evo.primitives, a.ast)

proc pointMutation*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## mutate a single node in the tree without altering any children
  template size: int = evo.tableau.tournamentSize
  template c: Primitives[T] = evo.primitives
  let a = tournament(evo, size).program
  result = some: c.newProgram pointMutation(evo.primitives, a.ast)

proc removeOneLeaf*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## remove a single terminal from the tree
  template size: int = evo.tableau.tournamentSize
  template c: Primitives[T] = evo.primitives
  let a = tournament(evo, size).program
  result = some: c.newProgram removeOneLeaf(evo.primitives, a.ast,
                                          size = evo.tableau.seedProgramSize)

proc appendOneLeaf*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## add a single terminal to a random parent in the tree
  template size: int = evo.tableau.tournamentSize
  template c: Primitives[T] = evo.primitives
  let a = tournament(evo, size).program
  result = some: c.newProgram appendOneLeaf(evo.primitives, a.ast,
                                          size = evo.tableau.seedProgramSize)

proc randomCrossover*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## perform crossover between a subtree and a novel tree constructed at random
  template size: int = evo.tableau.tournamentSize
  template c: Primitives[T] = evo.primitives
  let a = tournament(evo, size).program
  let b = randProgram(evo.primitives, size = evo.tableau.seedProgramSize)
  result = some: c.newProgram subtreeCrossover(a.ast, b.ast)

proc subtreeCrossover*[T, V](evo: var Evolver[T, V]): Option[Program[T]] =
  ## perform crossover between subtrees from two parents to form a new child
  template size: int = evo.tableau.tournamentSize
  template c: Primitives[T] = evo.primitives
  let a = tournament(evo, size).program
  let b = tournament(evo, size).program
  result = some: c.newProgram subtreeCrossover(a.ast, b.ast)
