import std/packedsets
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

const
  greadHoeffding* {.intdefine.} = 20
  defaultP* = 1.0 / greadHoeffding
  evolverCache = not programCache

type
  UnfitError* = object of ValueError
  FitOne*[T; V] = proc(q: T; s: SymbolSet[T, V]; p: Program[T]): Option[V] ##
  ## a fitness function that runs against a single symbolset

  FitMany*[T; V] = proc(q: T; iter: iterator(): (ptr SymbolSet[T, V], ptr V);
                        p: Program[T]): Option[V] ##
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
    targets: Option[seq[V]]
    weights: LPTab[int, float]
    balance: AliasMethod[int]
    core: CoreSpec
    tableau: Tableau
    population: Population[T]
    operators: AliasMethod[Operator[T, V]]
    gentime: MovingStat[float32]
    shorties: MovingStat[float32]
    novel: LPTab[Hash, PackedSet[int]]
    cache: LPTab[Hash, seq[Option[V]]]
    cacheCounter: int
    indexes: PackedSet[int]

proc platform*[T, V](evo: Evolver[T, V]): T =
  ## recover the platform of an evolver
  evo.platform

proc cacheUsage*(evo: Evolver): float =
  ## a metric on cache consumption for reporting
  if evo.dataset.len > 0 and evo.cache.len > 0:
    result = evo.cacheCounter.float / float(evo.cache.len * evo.dataset.len)

proc cacheSize*[T, V](evo: Evolver[T, V]): int =
  ## the total number of cached scores in the evolver
  evo.cachecounter

proc cacheSize*[T, V](evo: Evolver[T, V]; p: Program[T]): int =
  ## the number of cached scores measured for the program
  try:
    evo.novel[p.hash].len
  except KeyError:
    0

proc del*(evo: var Evolver; p: Program) =
  ## remove a program from the evolver; currently used to drop cache entries
  try:
    dec(evo.cacheCounter, evo.cacheSize(p))
    del(evo.cache, p.hash)
    del(evo.novel, p.hash)
  except KeyError:
    discard

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

proc resetCache*(evo: var Evolver) =
  ## clear the cache of previously-evaluated symbol sets, per each program
  init(evo.cache, initialSize = evo.tableau.maxPopulation)
  init(evo.novel, initialSize = evo.tableau.maxPopulation)
  clear evo.indexes
  for n in evo.dataset.low .. evo.dataset.high:
    evo.indexes.incl n

proc `dataset=`*[T, V](evo: var Evolver[T, V]; dataset: seq[SymbolSet[T, V]]) =
  ## assign a series of possible inputs
  evo.dataset = dataset
  # a complete reset of the dataset clears the program cache
  evo.resetCache()

proc `dataset=`*[T, V](evo: var Evolver[T, V];
                       dataset: seq[(SymbolSet[T, V], V)]) =
  ## assign a series of possible inputs, and their expected outputs
  evo.dataset = mapIt(dataset, it[0])
  evo.targets = some mapIt(dataset, it[1])

proc dataset*[T, V](evo: Evolver[T, V]): lent seq[SymbolSet[T, V]] =
  evo.dataset

proc `targets=`*[T, V](evo: var Evolver[T, V]; targets: seq[V]) =
  evo.targets = some targets

proc initEvolver*[T, V](evo: var Evolver[T, V]; platform: T; tableau: Tableau) =
  ## perform initial setup of the Evolver, binding platform and tableau
  evo = Evolver[T, V](platform: platform, tableau: tableau)
  evo.resetCache()

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

proc hasSampled(evo: Evolver; p: Program; index: int): bool =
  ## true if the program has been sampled with the symbol set at `index`
  try:
    result = index in evo.novel[p.hash]
  except KeyError:
    result = false

iterator sampleSets(evo: var Evolver; a, b: Program): PackedSet[int] =
  ## produce unordered sets of symbol set indices we should test
  var x, y: PackedSet[int]
  x = evo.novel.mgetOrPut(a.hash, x)
  y = evo.novel.mgetOrPut(b.hash, y)
  # first, make the comparisons more similar;
  # this has the side-effect of running only the missing samples, first
  yield symmetricDifference(x, y)

  # refresh our values because we trust no one
  x = evo.novel[a.hash]
  y = evo.novel[b.hash]

  # next, make them both more precise with novel samples
  yield evo.indexes - (x + y)

proc getScoreFromCache[T, V](evo: var Evolver[T, V]; p: Program;
                             index: int): Option[V] =
  ## load optional score using dataset[index] (symbol set)
  assert evo.hasSampled(p, index)
  try:
    result = evo.cache[p.hash][index]
  except KeyError:
    result = none V

proc addScoreToCache[T, V](evo: var Evolver[T, V]; p: Program; index: int;
                           score: Option[V]) =
  ## store optional score using dataset[index] (symbol set)
  demandValid score
  mixin strength
  var ps = evo.novel.mgetOrPut(p.hash, initPackedSet[int]())
  if not evo.novel[p.hash].containsOrIncl(index):
    inc evo.cacheCounter
    try:
      # omit it for now
      when false:
        var s: seq[Option[V]]
        s = evo.cache[p.hash]
        if index > s.high:
          # need to check the performance on these
          #setLen(s, max(s.len, index + 1))
          raise Defect.newException "disallowed at the moment"
        s[index] = score
      evo.cache[p.hash][index] = score
    except KeyError:
      var s: seq[Option[V]]
      newSeq(s, evo.dataset.len)  # work around cps `()`()
      s[index] = score
      evo.cache[p.hash] = s
    if score.isSome:
      p.push strength(get score)

proc scoreFromCache*[T, V](evo: var Evolver[T, V]; p: Program[T]): Option[V] =
  ## compute a new score for the program using prior evaluations
  if p.zombie: return none V

  if p.isNil:
    raise

  if p.hash == 0.Hash:
    raise

  if p.hash notin evo.cache or p.hash notin evo.novel:
    return none V

  let cache = addr evo.cache[p.hash]    # capture
  let novel = addr evo.novel[p.hash]    # capture
  let data = addr evo.dataset           # speed

  # setting up an iterator over populated cache entries
  iterator iter(): (ptr SymbolSet[T, V], ptr V) =
    for index in novel[].items:
      if cache[][index].isNone:
        raise Defect.newException "cache missing value"
      else:
        yield (addr data[][index], addr (get cache[][index]))

  # pass the iterator to fitmany() and check the result
  try:
    result = evo.fitmany(evo.platform, iter, p)
    demandValid result
  except UnfitError:
    result = none V

proc score*[T, V](evo: var Evolver[T, V]; index: int;
                  p: Program[T]): Option[V] =
  ## score the program against a single symbol set; we'll run the
  ## evolver's `fitone` function against the platform and data record
  ## to evaluate the program result. this scoring function also runs
  ## a stopwatch over `fitone()` and update's the program's runtime
  ## statistics.
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif p.zombie:
    return none V
  else:
    if evo.hasSampled(p, index):
      result = evo.getScoreFromCache(p, index)
    else:
      let began = getTime()
      result = evo.fitone(evo.platform, evo.dataset[index], p)
      demandValid result
      p.runtime.push (getTime() - began).inMilliseconds.float
      evo.addScoreToCache(p, index, result)

proc score*[T, V](evo: Evolver[T, V]; s: SymbolSet[T, V];
                  p: Program[T]): Option[V] {.deprecated: "use index".} =
  ## score the program against a single symbol set; if the program cache
  ## is enabled via its `programCache` constant, then we might simply
  ## fetch the score from there. otherwise, we'll run the evolver's
  ## `fitone` function against the platform and data record to evaluate
  ## the program. this scoring function also runs a stopwatch over
  ## `fitone()` and update's the programs runtime statistics.
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif p.zombie:
    return none V
  else:
    let began = getTime()
    result = evo.fitone(evo.platform, s, p)
    demandValid result
    p.runtime.push (getTime() - began).inMilliseconds.float

proc score*[T, V](evo: var Evolver[T, V]; dataset: seq[SymbolSet[T, V]];
                  p: Program[T]): Option[V] {.deprecated: "use greadFast".} =
  ## score a program against a series of symbol sets
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif evo.fitmany.isNil:
    raise ValueError.newException "evolver needs fitmany assigned"
  elif p.zombie:
    result = none V
  else:
    let e = addr evo
    # the iterator evaluates each symbol set in the dataset
    iterator iter(): (SymbolSet[T, V], V) =
      for index, ss in dataset.pairs:
        let s =
          when defined(greadFast):
            e[].score(index, p)
          else:
            e[].score(ss, p)
        if s.isNone:
          raise UnfitError.newException "fitone fail"
        else:
          e[].addScoreToCache(p, index, s)
          yield (ss, get s)

    # pass the iterator to fitmany() and check the result
    try:
      result = evo.fitmany(evo.platform, iter, p)
      demandValid result
    except UnfitError:
      result = none V

proc score*[T, V](evo: var Evolver[T, V]; p: Program[T]): Option[V] =
  ## score the program against all available symbol sets
  evo.score(evo.dataset, p)

proc scoreRandomly*[T, V](evo: var Evolver[T, V];
                          p: Program[T]): Option[V] =
  ## evaluate a program against a random symbol set; a smoke test
  evo.score(rand evo.dataset.high, p)

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

proc randomPop*[T, V](evo: var Evolver[T, V]): Population[T] =
  ## create a new (random) population using the given evolver's parameters
  mixin strength
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
          evo.scoreRandomly(p)
        else:
          evo.score(p)
      if s.isSome:
        p.score = strength(get s)
      else:
        p.score = NaN
      if not evo.tableau.requireValid or p.isValid:
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

proc confidentComparison*(evo: var Evolver; a, b: Program; p = defaultP): int =
  ## compare two programs with high confidence and minimal sampling

  # short-circuit on some obvious early insufficiencies
  if a.hash == b.hash: return 0
  if a.isValid != b.isValid:
    if a.isValid: return 1 else: return -1
  elif not a.isValid: return 0

  debug ""
  debug "it begins"
  # start with the scores as recovered from the cache
  var a1 = evo.scoreFromCache(a)
  var b1 = evo.scoreFromCache(b)

  var runs = 0
  template resample(p: Program; i: int; z: int): untyped {.dirty.} =
    ## if a program could move; let's test it further and maybe bail
    inc runs
    let score = evo.score(i, p)
    if score.isNone:
      debug "scored none; return " & $z
      return z

  template guard(): untyped {.dirty.} =
    ## short-circuit if either program is insufficient
    if a1.isNone:
      debug "a1 is none"
      return -1
    elif b1.isNone:
      debug "b1 is none"
      return 1

  const popscore = false

  block done:
    # we'll likely loop on two sample sets; the first is differing samples,
    # the second reflects symbol sets new to both programs
    for samples in sampleSets(evo, a, b):
      for i in samples.items:
        guard()  # ensure the programs score safely

        debug "pre-scaled: ", get a1, " and ", get b1
        when popscore:
          let sa = evo.population.score(get a1, a.len, get b1)
          let sb = evo.population.score(get b1, b.len, get a1)
        else:
          let sa = get a1
          let sb = get b1
        # XXX: when parsimony is on, it can change the sign of scores 🙄
        let d = abs(sa.float - sb.float)  # just wing it for now
        debug "    scores: ", sa, " and ", sb
        debug "delta is ", d

        # how probable is each program's score to move the given distance?
        let csa = evo.cacheSize(a)
        let csb = evo.cacheSize(b)
        let ha = hoeffding(csa, d)
        let hb = hoeffding(csb, d)

        if ha < p and hb < p:
          if runs > 0:
            debug "hoeffding ran ", runs, " saved ",
              percent(1.0 - (float(runs) / (samples.len.float*2.0)))
          break done

        # sample the index for any program that hasn't done so yet
        if ha > p and not evo.hasSampled(a, i):
          resample(a, i, -1)
          a1 = evo.scoreFromCache(a)
        if hb > p and not evo.hasSampled(b, i):
          resample(b, i, 1)
          b1 = evo.scoreFromCache(b)

  guard()  # ensure the programs score safely

  # order is unlikely to change; compare these scores
  when popscore:
    result = cmp(evo.population.score(get a1, a.len, get b1),
                 evo.population.score(get b1, b.len, get a1))
  else:
    # FIXME: better()?
    result = cmp(strength(get a1), strength(get b1))
  debug "returning cmp; ", a1.get, " vs ", b1.get, " = ", result
