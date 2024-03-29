import std/algorithm
import std/hashes
import std/logging
import std/math
import std/monotimes
import std/options
import std/random
import std/sequtils
import std/strformat
import std/times

import pkg/adix/lptabz
import pkg/adix/stat except variance, Option

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
import gread/genotype

const
  greadHoeffding* {.intdefine.} = 20
  defaultP* = 1.0 / greadHoeffding

type
  UnfitError* = object of ValueError
  FitOne*[T; V] = proc(q: T; s: SymbolSet[T, V]; p: var Program[T]): Option[V] ##
  ## a fitness function that runs against a single symbolset

  FitMany*[T; V] = proc(q: T; iter: iterator(): (ptr SymbolSet[T, V], ptr V);
                        p: Program[T]): Option[V] ##
  ## a fitness function that runs against a series of symbolsets

  Operator*[T, V] = proc(evo: var Evolver[T, V]): seq[Program[T]] {.nimcall.}
  OperatorWeight*[T, V] = tuple[operator: Operator[T, V]; weight: float64]

  Strength*[V] = proc(value: V): float ##
  ## a function converts program results to a float for sorting purposes

  LeanEvolver* = object of RootObj
    rng*: Rand                      # we essentially need to expose mutability
    tableau: Tableau

    name*: string                   # for reporting purposes
    core: CoreSpec
    birthday: MonoTime
    gentime: MovingStat[float32, uint32]
    shorties: MovingStat[float32, uint32]
    generations: Generation
    ken: PopMetrics

  HeavyEvolver*[T, V] = object of LeanEvolver
    grammar: Grammar
    operators: AliasMethod[Operator[T, V]]

    fittest: Option[Program[T]]

    platform: T
    dataset: seq[SymbolSet[T, V]]
    population: Population[T]

    strength: Strength[V]
    cache: GreadTable[Hash, seq[V]]
    cacheCounter: int
    indexes: GreadSet[int]
    unnovel: GreadTable[Hash, GreadSet[int]]
    scoreCache: GreadTable[Hash, Option[float]]

  OneEvolver*[T, V] = object of HeavyEvolver[T, V]
    fitone: FitOne[T, V]

  ManyEvolver*[T, V] = object of OneEvolver[T, V]
    fitmany: FitMany[T, V]

  Evolver*[T, V] = ManyEvolver[T, V]

proc paintScore*(evo: var Evolver; program: var Program; inPop = false): Score =
  ## do the score assignment dance
  mixin isValid
  let score = evo.score(program)
  program.score =
    if score.isSome:
      let str = evo.strength(get score)
      if str.isValid:
        str # XXX: rescale?
      else:
        NaN
    else:
      NaN
  if program.isValid:
    if inPop:
      evo.maybeResetFittest(program)
  else:
    program.zombie = true
  result = program.score

proc nextGeneration*(evo: var LeanEvolver): Generation =
  ## inform the evolver of a new generation
  inc evo.generations
  result = evo.generations

proc generation*(evo: LeanEvolver): Generation =
  ## return the current generation
  evo.generations

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

proc del*[T, V](evo: var Evolver[T, V]; p: Program[T]) =
  ## remove a program from the evolver; currently used to drop cache entries
  dec(evo.cacheCounter, evo.cacheSize(p))
  try:
    del(evo.cache, p.hash)
  except KeyError:
    discard
  try:
    del(evo.scoreCache, p.hash)
  except KeyError:
    discard
  try:
    del(evo.unnovel, p.hash)
  except KeyError:
    discard

proc shortGenome*(evo: Evolver): MovingStat[float32, uint32] =
  ## retrieve the statistics on mapping failures due to short genomes
  evo.shorties

proc shortGenome*(evo: var Evolver; tooShort: bool) =
  ## record a short (true) or sufficient (false) genome result due to mapping
  evo.shorties.push float(ord tooShort)

proc generationTime*(evo: LeanEvolver): MovingStat[float32, uint32] =
  ## fetch the generation time statistics
  evo.gentime

proc generationTime*(evo: var LeanEvolver; ms: float) =
  ## add a generation time (in millis) for recordkeeping
  evo.gentime.push ms

proc clearStats*(evo: var LeanEvolver) =
  ## reset any statistics recorded by the Evolver
  clear evo.gentime

when defined(greadReportFittestChanges):
  proc maybeReportFittest(evo: Evolver; p: Program) =
    if not p.isNil:
      notice fmt"fittest in {evo.core} score {p.score} from {p.core}/{p.generation} hash {p.hash}"
else:
  template maybeReportFittest(evo: Evolver; p: Program) = discard

proc maybeResetFittest*(evo: var Evolver; program: var Program) =
  ## maybe reset the fittest program metric
  if program.isValid and program.score.isValid:
    if (program.core.isNone or program.core == evo.core or evo.core.isNone) or evo.cacheSize(program) == evo.dataset.len:
      if evo.fittest.isNone or get(evo.fittest) < program:
        if evo.fittest.isSome:
          maybeReportFittest(evo, get(evo.fittest))
          if program.generation > get(evo.fittest).generation or program.core != get(evo.fittest).core:
            inc evo.ken.leaders
        program.flags.incl FinestKnown
        evo.fittest = some program
        maybeReportFittest(evo, program)

proc clearFittest*(evo: var Evolver) =
  reset evo.fittest

proc resetFittest*(evo: var Evolver) =
  ## reset the fittest program metric, clearing all other FinestKnown flags
  evo.clearFittest()
  for p in evo.population.mitems:
    evo.maybeResetFittest(p)
    p.flags.excl FinestKnown
  if evo.fittest.isSome:
    get(evo.fittest).flags.incl FinestKnown
    paintFittest(evo.ken, get(evo.fittest))

proc `operators=`*[T, V](evo: var Evolver[T, V];
                         weighted: openArray[(Operator[T, V], float64)]) =
  initAliasMethod(evo.operators, weighted)

proc `population=`*[T, V](evo: var Evolver[T, V]; population: Population[T]) =
  evo.population = population
  if not evo.population.isNil:
    for p in evo.population.mitems:
      discard evo.paintScore(p, inPop=false)  # ie. don't reset fittest
    if UseParsimony in evo.tableau:
      evo.toggleParsimony(on)
    else:
      evo.resetFittest()

proc population*[T, V](evo: Evolver[T, V]): Population[T] =
  evo.population

proc `grammar=`*[T, V](evo: var Evolver[T, V]; grammar: Grammar) =
  evo.grammar = grammar

proc grammar*[T, V](evo: Evolver[T, V]): Grammar =
  evo.grammar

proc resetCache*(evo: var Evolver) =
  ## clear the cache of previously-evaluated symbol sets, per each program
  initGreadTable(evo.cache, evo.tableau.maxPopulation)
  initGreadTable(evo.scoreCache, evo.tableau.maxPopulation)
  initGreadTable(evo.unnovel, evo.dataset.len)
  clear evo.indexes
  for index in evo.dataset.low .. evo.dataset.high:
    evo.indexes.incl index

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

proc initEvolver*(evo: var LeanEvolver; tableau: Tableau; rng: Rand = randState()) =
  ## perform initial setup of the Evolver, binding tableau
  evo.tableau = tableau
  evo.rng = rng
  evo.birthday = getMonoTime()

proc initEvolver*[T, V](evo: var Evolver[T, V]; platform: T; tableau: Tableau; rng: Rand = randState()) =
  ## perform initial setup of the Evolver, binding platform and tableau
  evo.initEvolver(tableau, rng)
  evo.platform = platform
  evo.resetCache()

proc chooseOperator*[T, V](evo: var Evolver[T, V]): Operator[T, V] =
  ## choose an operator at random
  if evo.operators.len == 0:
    raise ValueError.newException "evolver needs operators assigned"
  else:
    result = choose(evo.operators, evo.rng)

proc hasSampled*(evo: Evolver; p: Program; index: int): bool =
  ## true if the program has been sampled with the symbol set at `index`
  try:
    result = index in evo.unnovel[p.hash]
  except KeyError:
    result = false

iterator sampleSets(evo: var Evolver; a, b: Program): GreadSet[int] =
  ## produce unordered sets of symbol set indices we should test
  var x, y: GreadSet[int]
  initGreadSet x
  initGreadSet y
  x = evo.unnovel.mgetOrPut(a.hash, x)
  y = evo.unnovel.mgetOrPut(b.hash, y)

  when evo.unnovel is LPTabz:
    # refresh our values because LPTab be like that
    x = evo.unnovel[a.hash]
    y = evo.unnovel[b.hash]
  when not defined(release):
    doAssert x == evo.unnovel[a.hash]
    doAssert y == evo.unnovel[a.hash]

  # first, make the comparisons more similar;
  # this has the side-effect of running only the missing samples, first
  yield symmetricDifference(x, y)

  # next, make them both more precise with novel samples
  yield evo.indexes - (x + y)

proc getScoreFromCache*[T, V](evo: var Evolver[T, V]; p: Program;
                              index: int): ptr V =
  ## load score using dataset[index] (symbol set)
  try:
    if index <= evo.cache[p.hash].high:
      result = addr evo.cache[p.hash][index]
    else:
      raise IndexDefect.newException "index not yet cached"
  except KeyError:
    raise KeyError.newException "program not yet cached"

template initCache[T, V](evo: var Evolver[T, V]; p: Program[T]; index: int; score: V) =
  var s = newSeq[V](evo.dataset.len)
  s[index] = score
  evo.cache[p.hash] = s

proc addScoreToCache*[T, V](evo: var Evolver[T, V]; p: var Program; index: int;
                            score: V) =
  ## store score using dataset[index] (symbol set)
  demandValid score
  if p.hash notin evo.unnovel:
    var value: GreadSet[int]
    initGreadSet value
    evo.unnovel[p.hash] = value
  if not evo.unnovel[p.hash].containsOrIncl(index):
    inc evo.cacheCounter
    try:
      # grow the cache if necessary
      if evo.cache[p.hash].high < index:
        setLen(evo.cache[p.hash], index+1)
      evo.cache[p.hash][index] = score
    except KeyError:
      initCache(evo, p, index, score)
    assert evo.cache[p.hash].len <= evo.dataset.len
    assert not evo.strength.isNil, "strength() unassigned to evolver"
    p.push evo.strength(score)
    when not defined(release):
      doAssert index in evo.unnovel[p.hash]

proc scoreFromCache[T, V](evo: var Evolver[T, V]; p: var Program[T]): Option[V] =
  ## compute a new score for the program using prior evaluations
  if p.zombie: return none V

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
  except CatchableError:
    error p
    writeStackTrace()
    raise

proc score*[T, V](evo: var Evolver[T, V]; index: int;
                  p: var Program[T]): Option[V] =
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
      let began = getMonoTime()
      result = evo.fitone(evo.platform, evo.dataset[index], p)
      demandValid result
      p.pushRuntime (getMonoTime() - began).inNanoseconds.float
      if result.isSome:
        evo.addScoreToCache(p, index, get result)
      else:
        # XXX: can't wait to remove this...
        when V is float:
          evo.addScoreToCache(p, index, NaN)
        else:
          evo.addScoreToCache(p, index, default V)
        warn fmt"destroy score for index {index} program {p.hash}"

proc score[T, V](evo: Evolver[T, V]; ss: ptr SymbolSet[T, V];
                 p: var Program[T]): Option[V] {.deprecated: "use index".} =
  ## score the program against a single symbol set; we'll run the evolver's
  ## `fitone` function against the platform and data record to evaluate
  ## the program. this scoring function also runs a stopwatch over
  ## `fitone()` and updates the programs runtime statistics.
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif p.zombie:
    return none V
  else:
    let began = getMonoTime()
    result = evo.fitone(evo.platform, ss[], p)
    demandValid result
    p.pushRuntime (getMonoTime() - began).inNanoseconds.float

proc score*[T, V](evo: var Evolver[T, V]; indices: ptr GreadSet[int];
                  p: var Program[T]): Option[V] =
  ## score a program against a subset of symbol sets from the evolver
  mixin isValid
  mixin render
  if evo.fitone.isNil:
    raise ValueError.newException "evolver needs fitone assigned"
  elif evo.fitmany.isNil:
    raise ValueError.newException "evolver needs fitmany assigned"
  elif p.zombie:
    result = none V
  else:
    discard render p
    let p = addr p
    let e = addr evo
    # the iterator evaluates each symbol set in the dataset[indices]
    iterator iter(): (ptr SymbolSet[T, V], ptr V) =
      for index in indices[].items:
        var v: ptr V
        if e[].hasSampled(p[], index):
          v = e[].getScoreFromCache(p[], index)
        else:
          let s = e[].score(index, p[])
          if s.isNone:
            raise UnfitError.newException "fitone fail"
          else:
            v = addr (get s)
        yield (addr e[].dataset[index], v)

    # pass the iterator to fitmany() and check the result
    try:
      result = evo.fitmany(evo.platform, iter, p[])
      demandValid result
    except UnfitError:
      result = none V
    except CatchableError as e:
      error $e.name & ": " & e.msg
      writeStackTrace()
      raise

proc score*[T, V](evo: var Evolver[T, V]; p: var Program[T]): Option[V] =
  ## score the program against all available symbol sets
  evo.score(addr evo.indexes, p)

proc scoreRandomly*[T, V](evo: var Evolver[T, V];
                          p: var Program[T]): Option[V] =
  ## evaluate a program against a random symbol set; a smoke test
  if evo.dataset.len == 0:
    raise ValueError.newException "evolver lacks a dataset"
  else:
    evo.score(evo.rng.rand evo.dataset.high, p)

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
  when false:
    evo.toggleParsimony(UseParsimony in evo.tableau)
  else:
    {.warning: "we don't (yet) assert parsimony scoring on random pops".}
  while result.len < evo.tableau.seedPopulation:
    try:
      if evo.grammar.isNil:
        raise ValueError.newException "need grammar"
      var p = randProgram[T](evo.rng, evo.grammar, evo.tableau.seedProgramSize)
      p.core = evo.core
      # FIXME: optimization point
      let s = evo.score(p)
      p.score =
        if s.isSome:
          evo.strength(get s)
        else:
          NaN
      if RequireValid notin evo.tableau or p.isValid:
        result.add p
        when defined(greadEchoRandomPop):
          notice fmt"{result.len} {p}"
    except ShortGenome:
      discard
      #debug fmt"short genome on core {evo.core}; pop size {result.len}"
  info fmt"pop generation complete on core {evo.core}"

proc randomDataIndexes*(evo: var Evolver): seq[int] =
  ## a randomly-ordered sequence of dataset indexes
  result = newSeqOfCap[int](evo.dataset.len)
  for i in evo.dataset.low .. evo.dataset.high:
    result.add i
  shuffle(evo.rng, result)

proc confidentComparison*(evo: var Evolver; a, b: var Program; p = defaultP): int =
  ## compare two programs with high confidence and minimal sampling

  # short-circuit on some obvious early insufficiencies
  if a.hash == b.hash: return 0
  if a.isValid != b.isValid:
    if a.isValid: return 1 else: return -1
  elif not a.isValid: return 0

  var x, y: GreadSet[int]
  initGreadSet x
  initGreadSet y
  x = evo.unnovel.mgetOrPut(a.hash, x)
  y = evo.unnovel.mgetOrPut(b.hash, y)
  when not defined(release):
    doAssert x == evo.unnovel[a.hash]
    doAssert y == evo.unnovel[b.hash]

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

  block done:
    # we'll likely loop on two sample sets; the first is differing samples,
    # the second reflects symbol sets new to both programs
    for samples in sampleSets(evo, a, b):
      for i in samples.items:
        guard()  # ensure the programs score safely

        debug "pre-scaled: ", get a1, " and ", get b1
        let sa = evo.strength(get a1)
        let sb = evo.strength(get b1)
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
  # FIXME: better()?
  result = cmp(evo.strength(get a1), evo.strength(get b1))
  debug "returning cmp; ", a1.get, " vs ", b1.get, " = ", result

type
  Competitor[T] = tuple      # sorting by
    valid: bool              # validity, then by
    score: Score             # score, then by
    len: int                 # program length
    index: int
    program: Program[T]

proc discharge(evo: var Evolver; c: var Competitor) =
  ## a modern remover
  let s = evo.scoreFromCache(c.program)
  let score =
    if s.isSome:
      some evo.strength(get s)
    else:
      none float
  # update the population's metrics, etc.
  scoreChanged(evo.population, c.program, score, c.index)

proc tournament*[T, V](evo: var Evolver[T, V]; size: Positive;
                       order = Descending): Competitor[T] =
  ## v3 baby
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"

  # figure out the size of the tourney
  let size = max(1, min(evo.population.len, size))

  # the winner of each bout fights again
  var victim: Competitor[T]
  var seen: GreadSet[int]           # de-dupe fighters by index;
  initGreadSet seen
  # we're counting unique programs, not unique members!
  while seen.len < size:
    {.warning: "work around https://github.com/nim-lang/Nim/issues/19364".}
    let bug = randomMember(evo.population, evo.rng)
    if not seen.containsOrIncl bug.index:
      victim = (valid: bug.program.isValid, score: Score NaN,
                len: bug.program.len, index: bug.index, program: bug.program)
      if not result.program.isInitialized:
        # it's our first time through the loop, so we'll establish
        # the defender
        result = victim
      else:
        var cmp: int
        # we have an encumbent; see what's better when
        if false:
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

iterator trim*[T, V](evo: var Evolver[T, V]): Program[T] =
  ## emit the worst programs until the population is
  ## within the maximum defined by the tableau
  if evo.population.len > 0:
    while evo.population.len > evo.tableau.maxPopulation:
      let loser = tournament(evo, evo.population.len.Positive, order = Ascending)
      del(evo, loser.program)
      del(evo.population, loser.index)
      yield loser.program

func paintMetrics*(ken: var PopMetrics; evo: Evolver) =
  ## recover validity, score, and parsimony metrics of the population; O(n)
  ken = evo.ken
  paintMetrics(ken, evo.population)
  if UseParsimony notin evo.tableau:
    ken.parsimony = NaN
  if evo.fittest.isSome:
    paintFittest(ken, get(evo.fittest))

proc resetParsimony*(evo: var Evolver): PopMetrics =
  ## recompute the parsimony for the population,
  ## if parsimony is enabled for the population;
  ## returns the population metrics in any event
  paintMetrics(result, evo)
  try:
    if UseParsimony notin evo.tableau:
      return
    if evo.strength.isNil:
      raise ValueError.newException "evolver needs strength assigned"
    #profile "reset parsimony":
    block:
      # reset their scores
      for program in evo.population.mitems:
        var s: Option[float]
        try:
          s = evo.scoreCache[program.hash]
        except KeyError:
          let v = evo.score(program)
          if v.isSome:
            s = some evo.strength(get v)
            # XXX: s.isValid?
          else:
            s = none float
          evo.scoreCache[program.hash] = s
        if s.isSome and Score(get s).isValid:
          program.score = Score(get s)
          result.scores.push program.score.float
        else:
          program.zombie = true
          program.score = NaN
      # calculate parsimony
      result.parsimony = parsimony(result, evo.population)
      # we need to average adjusted scores as we compute them
      var modified: MovingStat[float64, uint32]
      # adjust the scores accordingly
      for program in evo.population.mitems:
        program.score = result.score(program.score, program.len)
        if program.score.isValid:
          modified.push program.score
      result.scores = modified
  finally:
    evo.ken = result
    #echo fmt"reset parsimony: {ff evo.ken.scores.mean} {ff evo.ken.parsimony}"

proc `strength=`*[T, V](evo: var Evolver[T, V]; strong: Strength[V]) =
  ## assign a new strength function to the evolver
  evo.strength = strong

proc strength*[T, V](evo: Evolver[T, V]): Strength[V] =
  ## the currently configured strength function for a single result value
  if evo.strength.isNil:
    raise ValueError.newException "evolver needs strength assigned"
  else:
    evo.strength

proc fittest*[T, V](evo: var Evolver[T, V]): var Option[Program[T]] =
  evo.fittest

proc fittest*[T, V](evo: Evolver[T, V]): Option[Program[T]] =
  evo.fittest

func resetMetrics*(evo: var Evolver): PopMetrics =
  ## recover validity, score, and parsimony metrics of the population; O(n)
  evo.ken.paintMetrics(evo)
  result = evo.ken

proc toggleParsimony*(evo: var Evolver; value = on) =
  ## turn parsimony `on` or `off`; when switching parsimony on,
  ## this will recompute and set the parsimony for the population
  if value == on:
    evo.tableau.flags.incl UseParsimony
  else:
    evo.tableau.flags.excl UseParsimony
  # recalculate parsimony and all other metrics
  discard resetParsimony(evo)
  # reset the fittest individual
  evo.resetFittest()

proc introduce*(evo: var Evolver; program: Program) =
  ## add a program to an evolver without scoring it
  evo.population.add program
  if program.core != evo.core:
    inc evo.ken.immigrants

proc add*(evo: var Evolver; program: var Program) =
  ## add a program to an evolver, maybe reset fittest program
  if not program.zombie and not program.score.isValid:
    discard evo.paintScore(program, inPop=false)
  evo.introduce program
  if program.isValid:
    evo.maybeResetFittest(program)

proc discover*(evo: var Evolver; program: Program) =
  inc evo.ken.inventions
  if program.zombie:
    inc evo.ken.zombies

proc add*[T, V](evo: var HeavyEvolver[T, V]; symbolset: sink SymbolSet[T, V]) =
  evo.dataset.add symbolset
  evo.indexes.incl evo.dataset.high

proc tableau*(evo: LeanEvolver): Tableau = evo.tableau

proc `core=`*(evo: var LeanEvolver; core: CoreSpec) =
  evo.core = core
  evo.ken.core = evo.core

proc core*(evo: LeanEvolver): CoreSpec = evo.core

proc rebirth*(evo: var LeanEvolver) =
  ## reset the evolver's birthday
  evo.birthday = getMonoTime()

proc rebirth*(evo: var LeanEvolver; generations: int) =
  ## reset the evolver's birthday and maximum generations
  rebirth evo
  evo.tableau.maxGenerations = generations

proc birthday*(evo: LeanEvolver): MonoTime = evo.birthday

proc indexLength*[T, V](evo: HeavyEvolver[T, V]): int =
  ## find the number of active dataset entries
  evo.indexes.len

proc discardIndex*[T, V](evo: var HeavyEvolver[T, V]; index: int) =
  ## discard an active dataset entry by index
  evo.indexes.excl index

proc clearScoreCache*[T, V](evo: var HeavyEvolver[T, V]; program: Program[T]) =
  try:
    evo.scoreCache.del(program.hash)
  except KeyError:
    discard

type
  GenomeEvolver*[T] = object of LeanEvolver
    operators: AliasMethod[GenomeOperatorSpec[T]]

proc chooseOperator*[T](evo: var GenomeEvolver[T]): GenomeOperatorSpec[T] =
  ## choose an operator at random
  if evo.operators.len == 0:
    raise ValueError.newException "evolver needs operators assigned"
  else:
    result = choose(evo.operators, evo.rng)

proc `operators=`*[T](evo: var GenomeEvolver[T];
                      weighted: openArray[(GenomeOperatorSpec[T], float64)]) =
  initAliasMethod(evo.operators, weighted)

proc tournament[T](evo: var GenomeEvolver[T]; size: Positive;
                    order = Descending): T =
  raise Defect.newException "impossible without a population"

proc tournament[T](evo: var GenomeEvolver[T]; order = Descending): T =
  raise Defect.newException "impossible without a population"
