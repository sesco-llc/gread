import std/algorithm
import std/hashes
import std/math
import std/options
import std/packedsets
import std/random
import std/sequtils
import std/strformat
import std/times

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
import gread/tournament

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

  Operator*[T, V] = proc(evo: var Evolver[T, V]): seq[Program[T]] {.nimcall.}
  OperatorWeight*[T, V] = tuple[operator: Operator[T, V]; weight: float64]

  Strength*[V] = proc(value: V): float ##
  ## a function converts program results to a float for sorting purposes

  Evolver*[T, V] = object
    platform: T
    rng*: Rand                      # we essentially need to expose mutability
    name*: string                   # for reporting purposes
    grammar: Grammar
    strength*: Strength[V]
    fitone: FitOne[T, V]
    fitmany: FitMany[T, V]
    dataset: seq[SymbolSet[T, V]]
    core: CoreSpec
    tableau: Tableau
    population: Population[T]
    operators: AliasMethod[Operator[T, V]]
    gentime: MovingStat[float32]
    shorties: MovingStat[float32]
    unnovel: LPTab[Hash, PackedSet[int]]
    cache: LPTab[Hash, seq[V]]
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
  evo.cacheCounter

proc cacheSize*[T, V](evo: Evolver[T, V]; p: Program[T]): int =
  ## the number of cached scores measured for the program
  result =
    try:
      evo.unnovel[p.hash].len
    except KeyError:
      0
  assert result <= evo.dataset.len

proc del*(evo: var Evolver; p: Program) =
  ## remove a program from the evolver; currently used to drop cache entries
  dec(evo.cacheCounter, evo.cacheSize(p))
  try:
    del(evo.cache, p.hash)
  except KeyError:
    discard
  try:
    del(evo.unnovel, p.hash)
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
  if not evo.population.isNil:
    for p in evo.population.items():
      if p.score.isNaN:
        # FIXME: optimization point
        let s =
          if evo.isEqualWeight:
            evo.scoreRandomly(p)
          else:
            evo.score(p)
        p.score =
          if s.isSome:
            evo.strength(get s)
          else:
            NaN
    if evo.tableau.useParsimony:
      evo.population.toggleParsimony(on)

proc population*[T, V](evo: Evolver[T, V]): Population[T] =
  evo.population

proc `grammar=`*[T, V](evo: var Evolver[T, V]; grammar: Grammar) =
  evo.grammar = grammar

proc grammar*[T, V](evo: Evolver[T, V]): Grammar =
  evo.grammar

proc resetCache*(evo: var Evolver) =
  ## clear the cache of previously-evaluated symbol sets, per each program
  init(evo.cache, initialSize = evo.tableau.maxPopulation)
  init(evo.unnovel, initialSize = evo.tableau.maxPopulation)
  clear evo.indexes
  for n in evo.dataset.low .. evo.dataset.high:
    evo.indexes.incl n

proc `dataset=`*[T, V](evo: var Evolver[T, V]; dataset: seq[SymbolSet[T, V]]) =
  ## assign a series of possible inputs
  evo.dataset = dataset
  # a complete reset of the dataset clears the program cache
  evo.resetCache()

proc dataset*[T, V](evo: Evolver[T, V]): lent seq[SymbolSet[T, V]] =
  if evo.dataset.high >= 0:
    result = evo.dataset
  else:
    raise ValueError.newException "evolver lacks a dataset"

proc initEvolver*[T, V](evo: var Evolver[T, V]; platform: T; tableau: Tableau; rng: Rand = randState()) =
  ## perform initial setup of the Evolver, binding platform and tableau
  evo = Evolver[T, V](platform: platform, tableau: tableau, rng: rng)
  evo.resetCache()

proc tableau*(evo: Evolver): Tableau = evo.tableau

proc isEqualWeight*(evo: Evolver): bool = evo.tableau.equalWeight

proc fittest*[T, V](evo: Evolver[T, V]): Option[Program[T]] =
  if not evo.population.isNil:
    if not evo.population.fittest.isNil:
      result = some evo.population.fittest

proc randomOperator*[T, V](evo: var Evolver[T, V]): Operator[T, V] =
  if evo.operators.len == 0:
    raise ValueError.newException "evolver needs operators assigned"
  else:
    choose(evo.operators, evo.rng)

proc hasSampled(evo: Evolver; p: Program; index: int): bool =
  ## true if the program has been sampled with the symbol set at `index`
  try:
    result = index in evo.unnovel[p.hash]
  except KeyError:
    result = false

iterator sampleSets(evo: var Evolver; a, b: Program): PackedSet[int] =
  ## produce unordered sets of symbol set indices we should test
  var x, y: PackedSet[int]
  x = evo.unnovel.mgetOrPut(a.hash, x)
  y = evo.unnovel.mgetOrPut(b.hash, y)
  # first, make the comparisons more similar;
  # this has the side-effect of running only the missing samples, first
  yield symmetricDifference(x, y)

  # refresh our values because we trust no one
  x = evo.unnovel[a.hash]
  y = evo.unnovel[b.hash]

  # next, make them both more precise with novel samples
  yield evo.indexes - (x + y)

proc getScoreFromCache[T, V](evo: var Evolver[T, V]; p: Program;
                             index: int): ptr V =
  ## load score using dataset[index] (symbol set)
  assert evo.hasSampled(p, index)
  try:
    result = addr evo.cache[p.hash][index]
  except KeyError:
    result = nil

proc initCache[T, V](evo: var Evolver[T, V]; p: Program; index: int; score: V) =
  var s = newSeq[V](evo.dataset.len)
  s[index] = score
  evo.cache[p.hash] = s

proc addScoreToCache[T, V](evo: var Evolver[T, V]; p: Program; index: int;
                           score: V) =
  ## store score using dataset[index] (symbol set)
  demandValid score
  var ps = evo.unnovel.mgetOrPut(p.hash, initPackedSet[int]())
  if not evo.unnovel[p.hash].containsOrIncl(index):
    inc evo.cacheCounter
    try:
      # omit it for now
      when false:
        var s: seq[V]
        s = evo.cache[p.hash]
        if index > s.high:
          # need to check the performance on these
          #setLen(s, max(s.len, index + 1))
          raise Defect.newException "disallowed at the moment"
        s[index] = score
      evo.cache[p.hash][index] = score
    except KeyError:
      initCache(evo, p, index, score)
    assert evo.cache[p.hash].len <= evo.dataset.len
    assert not evo.strength.isNil, "strength() unassigned to evolver"
    p.push evo.strength(score)

proc scoreFromCache*[T, V](evo: var Evolver[T, V]; p: Program[T]): Option[V] =
  ## compute a new score for the program using prior evaluations
  if p.zombie: return none V

  if p.isNil:
    raise Defect.newException "program is nil"

  if p.hash == 0.Hash:
    raise Defect.newException "program has no hash"

  if p.hash notin evo.cache or p.hash notin evo.unnovel:
    # no performance data available for the program;
    return none V

  let cache = addr evo.cache[p.hash]    # capture
  let unnovel = addr evo.unnovel[p.hash]    # capture
  let data = addr evo.dataset           # speed

  # setting up an iterator over populated cache entries
  iterator iter(): (ptr SymbolSet[T, V], ptr V) =
    for index in unnovel[].items:
      yield (addr data[][index], addr cache[][index])

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
  ## a stopwatch over `fitone()` and updates the program's runtime
  ## statistics.
  mixin isValid
  if index notin 0..evo.dataset.high:
    raise AssertionDefect.newException: "received bogus index"
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif p.zombie:
    return none V
  else:
    if evo.hasSampled(p, index):
      let v = evo.getScoreFromCache(p, index)
      result = some v[]
    else:
      let began = getTime()
      result = evo.fitone(evo.platform, evo.dataset[index], p)
      demandValid result
      p.runtime.push (getTime() - began).inMilliseconds.float
      if result.isSome:
        evo.addScoreToCache(p, index, get result)

proc score[T, V](evo: Evolver[T, V]; ss: ptr SymbolSet[T, V];
                 p: Program[T]): Option[V] {.deprecated: "use index".} =
  ## score the program against a single symbol set; if the program cache
  ## is enabled via its `programCache` constant, then we might simply
  ## fetch the score from there. otherwise, we'll run the evolver's
  ## `fitone` function against the platform and data record to evaluate
  ## the program. this scoring function also runs a stopwatch over
  ## `fitone()` and updates the programs runtime statistics.
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif p.zombie:
    return none V
  else:
    let began = getTime()
    result = evo.fitone(evo.platform, ss[], p)
    demandValid result
    p.runtime.push (getTime() - began).inMilliseconds.float

proc score*[T, V](evo: var Evolver[T, V]; indices: ptr PackedSet[int];
                  p: Program[T]): Option[V] =
  ## score a program against a subset of symbol sets from the evolver
  mixin isValid
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif evo.fitmany.isNil:
    raise ValueError.newException "evolver needs fitmany assigned"
  elif p.zombie:
    result = none V
  else:
    let e = addr evo
    # the iterator evaluates each symbol set in the dataset
    iterator iter(): (ptr SymbolSet[T, V], ptr V) =
      for index in indices[].items:
        var v: ptr V
        if e[].hasSampled(p, index):
          v = e[].getScoreFromCache(p, index)
        else:
          let s = e[].score(index, p)
          if s.isNone:
            raise UnfitError.newException "fitone fail"
          else:
            v = unsafeAddr (get s)
        yield (unsafeAddr e[].dataset[index], v)

    # pass the iterator to fitmany() and check the result
    try:
      result = evo.fitmany(evo.platform, iter, p)
      demandValid result
    except UnfitError:
      result = none V

proc score*[T, V](evo: var Evolver[T, V]; p: Program[T]): Option[V] =
  ## score the program against all available symbol sets
  mixin isValid
  evo.score(addr evo.indexes, p)

proc scoreRandomly*[T, V](evo: var Evolver[T, V];
                          p: Program[T]): Option[V] =
  ## evaluate a program against a random symbol set; a smoke test
  if evo.dataset.len == 0:
    raise ValueError.newException "evolver lacks a dataset"
  else:
    evo.score(evo.rng.rand evo.dataset.high, p)

proc `strength=`*[T, V](evo: var Evolver[T, V]; strength: Strength[V]) =
  ## assign a new strength function to the evolver
  evo.strength = strength

proc `fitone=`*[T, V](evo: var Evolver[T, V]; fitter: FitOne[T, V]) =
  ## assign a new fitness function to the evolver
  evo.fitone = fitter
  evo.resetCache()

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
    evo.dataset[evo.rng.rand evo.dataset.high]

proc randomPop*[T, V](evo: var Evolver[T, V]): Population[T] =
  ## create a new (random) population using the given evolver's parameters
  mixin isValid
  result = newPopulation[T](evo.tableau.seedPopulation, core = evo.core)
  result.toggleParsimony(evo.tableau.useParsimony)
  while result.len < evo.tableau.seedPopulation:
    try:
      let p =
        if not evo.grammar.isNil:
          randProgram[T](evo.rng, evo.grammar, evo.tableau.seedProgramSize)
        else:
          raise ValueError.newException "need grammar"
      p.core = evo.core
      # FIXME: optimization point
      let s =
        if evo.isEqualWeight:
          evo.scoreRandomly(p)
        else:
          evo.score(p)
      p.score =
        if s.isSome:
          evo.strength(get s)
        else:
          NaN
      if not evo.tableau.requireValid or p.isValid:
        result.add p
        when defined(greadEchoRandomPop):
          echo result.len, " ", $p
    except ShortGenome:
      discard
      #echo "short genome on core ", evo.core, "; pop size ", result.len
  echo "pop generation complete on core ", evo.core

proc randomDataIndexes*(evo: var Evolver): seq[int] =
  ## a randomly-ordered sequence of dataset indexes
  result = newSeqOfCap[int](evo.dataset.len)
  for i in evo.dataset.low .. evo.dataset.high:
    result.add i
  shuffle(evo.rng, result)

proc confidentComparison*(evo: var Evolver; a, b: Program; p = defaultP): int =
  ## compare two programs with high confidence and minimal sampling

  # short-circuit on some obvious early insufficiencies
  if a.hash == b.hash: return 0
  if a.isValid != b.isValid:
    if a.isValid: return 1 else: return -1
  elif not a.isValid: return 0

  var x, y: PackedSet[int]
  x = evo.unnovel.mgetOrPut(a.hash, x)
  y = evo.unnovel.mgetOrPut(b.hash, y)

  var sampled = x * y

  debug ""
  debug "it begins"
  # start with the scores of training results both programs were run with
  var a1 = evo.score(addr sampled, a)
  var b1 = evo.score(addr sampled, b)

  var runs = 0
  template resample(p: Program; i: int; z: int): untyped {.dirty.} =
    ## if a program could move; let's test it further and maybe bail
    inc runs
    sampled.incl i
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
          let sa = evo.strength evo.population.score(get a1, a.len, get b1)
          let sb = evo.strength evo.population.score(get b1, b.len, get a1)
        else:
          let sa = evo.strength get a1
          let sb = evo.strength get b1
        # XXX: when parsimony is on, it can change the sign of scores 🙄
        let d = abs(sa.float - sb.float)  # just wing it for now
        debug "    scores: ", sa, " and ", sb
        debug "delta is ", d

        # how probable is each program's score to move the given distance?
        let ha = hoeffding(sampled.len, d)
        let hb = hoeffding(sampled.len, d)

        if ha < p and hb < p:
          if false and runs > 0:
            debug "hoeffding ran ", runs, " saved ",
              percent(1.0 - (float(runs) / (evo.indexes.len.float*2.0)))
          break done

        # sample the index for any program that hasn't done so yet
        if ha > p and not evo.hasSampled(a, i):
          resample(a, i, -1)
          a1 = evo.score(addr evo.unnovel[a.hash], a)
        if hb > p and not evo.hasSampled(b, i):
          resample(b, i, 1)
          b1 = evo.score(addr evo.unnovel[b.hash], b)

  guard()  # ensure the programs score safely

  # order is unlikely to change; compare these scores
  when popscore:
    result = cmp(evo.population.score(get a1, a.len, get b1),
                 evo.population.score(get b1, b.len, get a1))
  else:
    # FIXME: better()?
    result = cmp(evo.strength(get a1), evo.strength(get b1))
  debug "returning cmp; ", a1.get, " vs ", b1.get, " = ", result

proc remover[T, V](evo: var Evolver[T, V];
                   competitors: var seq[Competitor[T]]; i: int) =
  ## used to update the population with a score change prior to removal
  let c = competitors[i]
  # FIXME: optimization point
  let s =
    # XXX: the problem here is that the score may well be -0.0 if the (few?)
    #      datapoints yield an impressive score
    if evo.isEqualWeight:
      evo.scoreFromCache(c.program)
    else:
      evo.score(c.program)
  let score =
    if s.isSome:
      some evo.strength(get s)
    else:
      none float
  #debug "rm ", i, " t-score ", c.score, " was ", c.program.score, " now ", score
  when false:
    if c.program.zombie:
      # raise
      discard
    else:
      evo.population.scoreChanged(c.program, score, c.index)
  elif true:
    evo.population.scoreChanged(c.program, score, c.index)
  else:
    qualityTrackIt(evo.population, c.program, c.program.score):
      it = s
  del(competitors, i)

proc discharge(evo: var Evolver; c: Competitor) =
  ## a modern remover
  when false:
    if c.program.zombie:
      # raise
      discard
    else:
      let s = evo.scoreFromCache(c.program)
      let score =
        if s.isSome:
          some evo.strength(get s)
        else:
          none float
      if c.program.zombie:
        discard
      else:
        scoreChanged(evo.population, c.program, score, c.index)
  elif true:
    let s = evo.scoreFromCache(c.program)
    let score =
      if s.isSome:
        some evo.strength(get s)
      else:
        none float
    scoreChanged(evo.population, c.program, score, c.index)
  else:
    qualityTrackIt(evo.population, c.program, c.program.score):
      it = evo.quality evo.scoreFromCache(c.program)
  if evo.cacheSize(c.program) == evo.dataset.len:
    maybeResetFittest(evo.population, c.program)

proc tournament*[T, V](evo: var Evolver[T, V]; size: int;
                       order = Descending): Competitor[T] =
  ## v3 baby
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"
  if size < 1:
    raise ValueError.newException:
      "cannot run a tournament with less than one competitor"

  # figure out the size of the tourney
  let size = max(1, min(evo.population.len, size))

  # the winner of each bout fights again
  var victim: Competitor[T]
  var seen: PackedSet[int]           # de-dupe fighters by index;
  # we're counting unique programs, not unique members!
  while seen.len < size:
    var (i, p) = randomMember(evo.population, evo.rng)
    if not seen.containsOrIncl i:
      victim = (valid: p.isValid, score: Score NaN,
                len: p.len, index: i, program: p)
      if result.program.isNil:
        # it's our first time through the loop, so we'll establish
        # the defender
        result = victim
      else:
        var cmp: int
        # we have an encumbent; see what's better when
        if evo.isEqualWeight:
          cmp = confidentComparison(evo, victim.program, result.program)
        else:
          # XXX: temporary hack?  needs to be profiled...
          let v = evo.score(victim.program)
          if v.isNone:
            # choose the encumbent
            cmp = -1
          else:
            # score the encumbent
            let r = evo.score(result.program)
            if r.isNone:
              # choose the victim
              cmp = 1
            else:
              # choose the weaker
              cmp = system.cmp(evo.strength(get v), evo.strength(get r))
        if cmp == -1 and order == Ascending:
          discharge(evo, result)
          result = victim
        elif cmp == 1 and order == Descending:
          discharge(evo, result)
          result = victim
        else:
          discharge(evo, victim)

  # reset the score of the winner only if necessary
  discharge(evo, result)
  result.valid = result.program.isValid
  result.score = result.program.score

  debug ""
  debug "tournament result: ", result
  debug "actual score of winner: ", result.program.score
  debug "order ", order
  think result

iterator trim*[T, V](evo: var Evolver[T, V]): Program[T] =
  ## emit the worst programs until the population is
  ## within the maximum defined by the tableau
  while evo.population.len > evo.tableau.maxPopulation:
    let loser = tournament(evo, evo.population.len, order = Ascending)
    del(evo, loser.program)
    del(evo.population, loser.index)
    yield loser.program
