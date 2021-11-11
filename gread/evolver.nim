import std/times
import std/sequtils
import std/random
import std/options

import pkg/adix/lptabz
import pkg/adix/stat

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
    primitives: Primitives[T]
    population: Population[T]
    operators: AliasMethod[Operator[T, V]]
    gentime: MovingStat[float32]

proc generationTime*(evo: Evolver): MovingStat[float32] =
  ## fetch the generation time statistics
  evo.gentime

proc generationTime*(evo: var Evolver; ms: float) =
  ## add a generation time (in millis) for recordkeeping
  evo.gentime.push ms

proc clearStats*(evo: var Evolver) =
  ## reset any statistics recorded by the Evolver
  clear evo.gentime

proc `core=`*(evo: var Evolver; core: CoreSpec) = evo.core = core
proc core*(evo: var Evolver): CoreSpec = evo.core

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

proc dataset*[T, V](evo: Evolver[T, V]): lent seq[SymbolSet[T, V]] =
  evo.dataset

proc `targets=`*[T, V](evo: var Evolver[T, V]; targets: seq[Score]) =
  evo.targets = some targets

proc initEvolver*[T, V](evo: var Evolver[T, V]; platform: T; tableau: Tableau) =
  ## perform initial setup of the Evolver, binding platform and tableau
  evo = Evolver[T, V](platform: platform, tableau: tableau)

proc tableau*(evo: Evolver): Tableau = evo.tableau

proc fittest*[T, V](evo: Evolver[T, V]): Option[Program[T]] =
  if not evo.population.isNil:
    if not evo.population.fittest.isNil:
      result = some evo.population.fittest

proc randomOperator*[T, V](evo: Evolver[T, V]): Operator[T, V] =
  if evo.operators.len == 0:
    raise ValueError.newException "evolver needs operators assigned"
  else:
    choose evo.operators

proc score*[T, V](evo: Evolver[T, V]; s: SymbolSet[T, V];
                  p: Program[T]): Option[Score] =
  ## score the program against a single symbol set; if the program cache
  ## is enabled via its `programCache` constant, then we might simply
  ## fetch the score from there. otherwise, we'll run the evolver's
  ## `fitone` function against the platform and data record to evaluate
  ## the program. this scoring function also runs a stopwatch over
  ## `fitone()` and update's the programs runtime statistics.
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  else:
    result = p.getScoreFromCache(s.hash)
    if result.isNone:
      let began = getTime()
      result = evo.fitone(evo.platform, s, p)
      p.runtime.push (getTime() - began).inMilliseconds.float
      if result.isSome:
        p.addScoreToCache(s.hash, get result)

proc collectCachedScores[T, V](evo: Evolver[T, V]; p: Program[T]): seq[(SymbolSet[T, V], Score)] =
  when programCache:
    result = newSeqOfCap[(SymbolSet[T, V], Score)](evo.dataset.len)
    for ss in evo.dataset.items:
      let s = getScoreFromCache(p, ss.hash)
      if s.isSome:
        result.add (ss, get s)

proc scoreFromCache*[T, V](evo: Evolver[T, V]; p: Program[T]): Option[Score] =
  profile "score from cache":
    let cached = collectCachedScores(evo, p)
    if cached.len > 0:
      result = evo.fitmany(evo.platform, cached, p)

proc score*[T, V](evo: Evolver[T, V]; dataset: seq[SymbolSet[T, V]];
                  p: Program[T]): Option[Score] =
  ## score a program against a series of symbol sets
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  if evo.fitmany.isNil:
    raise ValueError.newException "evolver needs fitmany assigned"
  else:
    block exit:
      var scores = newSeqOfCap[(SymbolSet[T, V], Score)](dataset.len)
      for ss in dataset.items:
        let s = evo.score(ss, p)
        if s.isNone:
          result = none Score
          break exit
        else:
          scores.add (ss, get s)
      result = evo.fitmany(evo.platform, scores, p)

proc score*[T, V](evo: Evolver[T, V]; p: Program[T]): Option[Score] =
  ## score the program against all available symbol sets
  let s = evo.score(evo.dataset, p)
  if s.isSome:
    p.score = get s
    # FIXME: find a better way to do this (ie. discover the index somehow?)
    when false:
      if not evo.population.isNil:
        scoreChanged(evo.population, p, p.score, index = none int)  # 😢
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
    p.core = evo.core
    result.add p
