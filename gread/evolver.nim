import std/sequtils
import std/random
import std/options

import pkg/adix/lptabz

import gread/population
import gread/spec
import gread/ast
import gread/programs
import gread/data
import gread/aliasmethod
import gread/fertilizer
import gread/tableau
import gread/primitives

type
  FitOne*[T; V] = proc(q: T; s: SymbolSet[T, V]; p: Program[T]): Option[Score] ##
  ## a fitness function that runs against a single symbolset

  FitMany*[T; V] = proc(q: T; ss: openArray[(SymbolSet[T, V], Score)];
                        p: Program[T]): Option[Score] ##
  ## a fitness function that runs against a series of symbolsets

  Operator*[T, V] = proc(pop: var Evolver[T, V]): Option[Program[T]] {.nimcall.}
  Weight = float or float64
  OperatorWeight*[T, V] = tuple[operator: Operator[T, V]; weight: float64]

  Evolver*[T, V] = object
    platform: T
    fitone: FitOne[T, V]
    fitmany: FitMany[T, V]
    dataset: seq[SymbolSet[T, V]]
    targets: Option[seq[Score]]
    weights: LPTab[int, float]
    balance: AliasMethod[int]
    core: CoreSpec
    tableau: Tableau
    fitness: Fitness[T]
    primitives: Primitives[T]
    population: Population[T]
    operators: AliasMethod[Operator[T, V]]

proc `core=`*[T, V](evo: var Evolver[T, V]; core: CoreSpec) =
  evo.core = core

proc `operators=`*[T, V](evo: var Evolver[T, V];
                         weighted: openArray[(Operator[T, V], float64)]) =
  initAliasMethod(evo.operators, weighted)

proc `primitives=`*[T, V](evo: var Evolver[T, V]; primitives: Primitives[T]) =
  evo.primitives = primitives

proc primitives*[T, V](evo: Evolver[T, V]): Primitives[T] =
  evo.primitives

proc `population=`*[T, V](evo: var Evolver[T, V]; population: Population[T]) =
  evo.population = population

proc population*[T, V](evo: Evolver[T, V]): Population[T] =
  evo.population

proc `dataset=`*[T, V](evo: var Evolver[T, V]; dataset: seq[SymbolSet[T, V]]) =
  ## assign a series of possible inputs
  evo.dataset = dataset

proc `dataset=`*[T, V](evo: var Evolver[T, V]; dataset: seq[(SymbolSet[T, V], Score)]) =
  ## assign a series of possible inputs, and their expected outputs
  evo.dataset = mapIt(dataset, it[0])
  evo.targets = some mapIt(dataset, it[1])

proc `targets=`*[T, V](evo: var Evolver[T, V]; targets: seq[Score]) =
  evo.targets = some targets

proc initEvolver*[T, V](evo: var Evolver[T, V]; platform: T; tableau: Tableau) =
  ## perform initial setup of the Evolver, binding platform and tableau
  evo = Evolver[T, V](platform: platform, tableau: tableau)

proc tableau*(evo: Evolver): Tableau = evo.tableau

proc fittest*[T, V](evo: Evolver[T, V]): Option[Program[T]] =
  if not evo.population.isNil:
    result = some evo.population.fittest

proc randomOperator*[T, V](evo: Evolver[T, V]): Operator[T, V] =
  if evo.operators.len == 0:
    raise ValueError.newException "evolver needs operators assigned"
  else:
    choose evo.operators

proc score*[T, V](evo: Evolver[T, V]; s: SymbolSet[T, V];
                  p: Program[T]): Option[Score] =
  ## score the program against a single symbol set
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  else:
    result = evo.fitone(evo.platform, s, p)

proc score*[T, V](evo: Evolver[T, V]; p: Program[T]): Option[Score] =
  ## score the program against all available symbol sets
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  if evo.fitmany.isNil:
    raise ValueError.newException "evolver needs fitmany assigned"
  else:
    block exit:
      var scores = newSeqOfCap[(SymbolSet[T, V], Score)](evo.dataset.len)
      for ss in evo.dataset.items:
        let s = evo.score(ss, p)
        if s.isNone:
          result = none Score
          break exit
        else:
          scores.add (ss, get s)
      let s = evo.fitmany(evo.platform, scores, p)
      if s.isSome:
        result = s

proc `fitone=`*[T, V](evo: var Evolver[T, V]; fitter: FitOne[T, V]) =
  ## assign a new fitness function to the evolver
  evo.fitone = fitter

proc fitone*[T, V](evo: Evolver[T, V]): FitOne[T, V] =
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  else:
    evo.fitone

proc `fitmany=`*[T, V](evo: var Evolver[T, V]; fitter: FitMany[T, V]) =
  ## assign a new fitness function to the evolver
  evo.fitmany = fitter

proc fitmany*[T, V](evo: Evolver[T, V]): FitMany[T, V] =
  if evo.fitmany.isNil:
    raise ValueError.newException "evolver needs fitmany assigned"
  else:
    evo.fitmany

proc randomSymbols*[T, V](evo: Evolver[T, V]): SymbolSet[T, V] =
  if evo.dataset.len == 0:
    raise ValueError.newException "evolver needs dataset assigned"
  else:
    evo.dataset[rand evo.dataset.high]

proc randomPop*[T, V](evo: Evolver[T, V]): Population[T] =
  ## create a new (random) population using the given evolver's parameters
  result = newPopulation[T](evo.tableau.seedPopulation, core = evo.core)
  while result.len < evo.tableau.seedPopulation:
    let p = randProgram(evo.primitives, evo.tableau.seedProgramSize)
    let s = evo.score(p) #evo.randomSymbols(), p)
    if s.isSome:
      if s.get.isNaN:
        raise ValueError.newException:
          "a fitness function produced NaN; this is unsupported for randomPop()"
      else:
        p.score = get s
        result.add p
