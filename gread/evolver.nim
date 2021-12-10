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
import gread/grammar
import gread/maths

type
  FitOne*[T; V] = proc(q: T; s: SymbolSet[T, V]; p: Program[T]): Option[Score] ##
  ## a fitness function that runs against a single symbolset

  FitMany*[T; V] = proc(q: T; ss: openArray[(SymbolSet[T, V], Score)];
                        p: Program[T]): Option[Score] ##
  ## a fitness function that runs against a series of symbolsets

  Operator*[T, V] = proc(pop: var Evolver[T, V]): Option[Program[T]] {.nimcall.}
  #Weight = float or float64
  OperatorWeight*[T, V] = tuple[operator: Operator[T, V]; weight: float64]

  Evolver*[T, V] = object
    platform: T
    grammar: Grammar[T]
    fitone: FitOne[T, V]
    fitmany: FitMany[T, V]
    dataset: seq[SymbolSet[T, V]]
    targets: Option[seq[Score]]
    weights: LPTab[int, float]
    balance: AliasMethod[int]
    core: CoreSpec
    tableau: Tableau
    population: Population[T]
    operators: AliasMethod[Operator[T, V]]
    gentime: MovingStat[float32]
    shorties: MovingStat[float32]

proc platform*[T, V](evo: Evolver[T, V]): T =
  ## recover the platform of an evolver
  evo.platform

proc shortGenome*(evo: Evolver): MovingStat[float32] =
  ## retrieve the statistics on mapping failures due to short genomes
  evo.shorties

proc shortGenome*(evo: var Evolver; tooShort: bool) =
  ## record a short (true) or sufficient (false) genome result due to mapping
  evo.shorties.push float(ord tooShort)

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

proc `population=`*[T, V](evo: var Evolver[T, V]; population: Population[T]) =
  evo.population = population

proc population*[T, V](evo: Evolver[T, V]): Population[T] =
  evo.population

proc `grammar=`*[T, V](evo: var Evolver[T, V]; grammar: Grammar[T]) =
  evo.grammar = grammar

proc grammar*[T, V](evo: Evolver[T, V]): Grammar[T] =
  evo.grammar

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
      p.addScoreToCache(s.hash, result)

proc collectCachedScores[T, V](evo: Evolver[T, V];
                               p: Program[T]): seq[(SymbolSet[T, V], Score)] =
  ## select input and score pairs from prior evaluations of the program
  if not p.zombie:
    when programCache:
      result = newSeqOfCap[(SymbolSet[T, V], Score)](evo.dataset.len)
      for ss in evo.dataset.items:
        let s = p.getScoreFromCache(ss.hash)
        if s.isSome:
          result.add (ss, get s)

proc scoreFromCache*[T, V](evo: Evolver[T, V]; p: Program[T]): Option[Score] =
  ## compute a new score for the program using prior evaluations
  if not p.zombie:
    profile "score from cache":
      let cached = collectCachedScores(evo, p)
      if cached.len > 0:
        result = evo.fitmany(evo.platform, cached, p)

proc score*[T, V](evo: Evolver[T, V]; dataset: seq[SymbolSet[T, V]];
                  p: Program[T]): Option[Score] =
  ## score a program against a series of symbol sets
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif evo.fitmany.isNil:
    raise ValueError.newException "evolver needs fitmany assigned"
  elif p.zombie:
    result = none Score
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
  evo.score(evo.dataset, p)

proc `fitone=`*[T, V](evo: var Evolver[T, V]; fitter: FitOne[T, V]) =
  ## assign a new fitness function to the evolver
  evo.fitone = fitter

proc fitone*[T, V](evo: Evolver[T, V]): FitOne[T, V] =
  ## the currently configured fitness function for a single symbol set
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  else:
    evo.fitone

proc `fitmany=`*[T, V](evo: var Evolver[T, V]; fitter: FitMany[T, V]) =
  ## assign a new fitness function to the evolver
  evo.fitmany = fitter

proc fitmany*[T, V](evo: Evolver[T, V]): FitMany[T, V] =
  ## the currently configured fitness function for multiple outputs
  if evo.fitmany.isNil:
    raise ValueError.newException "evolver needs fitmany assigned"
  else:
    evo.fitmany

proc randomSymbols*[T, V](evo: Evolver[T, V]): SymbolSet[T, V] =
  ## select a random symbol set from the dataset
  if evo.dataset.len == 0:
    raise ValueError.newException "evolver needs dataset assigned"
  else:
    evo.dataset[rand evo.dataset.high]

proc randomPop*[T, V](evo: Evolver[T, V]): Population[T] =
  ## create a new (random) population using the given evolver's parameters
  result = newPopulation[T](evo.tableau.seedPopulation, core = evo.core)
  result.toggleParsimony(evo.tableau.useParsimony)
  while result.len < evo.tableau.seedPopulation:
    try:
      let p =
        if not evo.grammar.isNil:
          randProgram(evo.grammar, evo.tableau.seedProgramSize)
        else:
          raise ValueError.newException "need grammar"
      p.core = evo.core
      # FIXME: optimization point
      let s =
        when defined(greadFast):
          evo.score(evo.randomSymbols, p)
        else:
          evo.score(p)
      if s.isSome:
        p.score = get s
      else:
        p.score = NaN
      if not evo.tableau.requireValid or (not p.zombie and p.score.isValid):
        result.add p
    except ShortGenome:
      discard
      #echo "short genome on core ", evo.core, "; pop size ", result.len
  echo "pop generation complete on core ", evo.core

proc randomDataIndexes*(evo: Evolver): seq[int] =
  ## a randomly-ordered sequence of dataset indexes
  result = newSeqOfCap[int](evo.dataset.len)
  for i in evo.dataset.low .. evo.dataset.high:
    result.add i
  shuffle result

proc confidentComparison*(evo: Evolver; samples: seq[int];
                          a, b: Program; p = 0.05): int =
  ## compare two programs with high confidence and minimal sampling
  if samples.len == 0:
    raise Defect.newException "gimme samples"

  # short-circuit on some obvious early insufficiencies
  if a.hash == b.hash: return 0
  if a.isValid != b.isValid:
    if a.isValid: return 1 else: return -1
  elif not a.isValid: return 0

  template resample(x: Program; s: untyped; z: int): untyped {.dirty.} =
    ## if a program could move; let's test it further and maybe bail
    if evo.score(s, x).isNone:
      debug "scored none; return " & $z
      return z

  debug ""
  debug "it begins"
  var a1, b1: Option[Score]
  for i in samples.items:
    template sample: untyped = evo.dataset[samples[i]]

    # discover the current score of each program
    a1 = evo.scoreFromCache(a)
    b1 = evo.scoreFromCache(b)

    # short-circuit if either program is insufficient
    if a1.isNone:
      debug "a1 is none"
      return -1
    elif b1.isNone:
      debug "b1 is none"
      return 1

    # find the difference in scores, with parsimony, rescaled to 0-1
    debug "pre-scaled: ", get a1, " and ", get b1
    let sa = evo.population.score(get a1, a.len, get b1)
    let sb = evo.population.score(get b1, b.len, get a1)
    let d = abs(sa.float - sb.float)
    debug "    scores: ", sa, " and ", sb
    debug "delta is ", d

    # how probable is each program's score to move the given distance?
    let ha = hoeffding(a.cacheSize, d)
    let hb = hoeffding(b.cacheSize, d)
    debug "the hoff says: ", Score ha, " and ", Score hb

    if ha > p and a.cacheSize < samples.len:
      resample(a, sample, -1)
    elif hb > p and b.cacheSize < samples.len:
      resample(b, sample, 1)
    else:
      debug "hoeffding saved ", (samples.len*2) - (a.cacheSize + b.cacheSize)
      break

  # order is unlikely to change; scale and compare these scores
  debug "returning cmp"
  result = cmp(evo.population.score(get a1, a.len, get b1),
               evo.population.score(get b1, b.len, get a1))
