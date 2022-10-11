import std/hashes
import std/json
import std/logging
import std/math
import std/monotimes
import std/options
import std/os
import std/random
import std/sequtils
import std/strformat
import std/strutils
import std/times

import pkg/lunacy except Integer
import pkg/lunacy/json as luajs
import pkg/adix/lptabz
import pkg/adix/stat
import pkg/balls
import pkg/cps
import pkg/frosty/streams as brrr
import pkg/htsparse/fennel/fennel_core_only as parsefen
import pkg/lrucache

import gread/spec
import gread/ast
import gread/population
import gread/programs
import gread/maths except variance
import gread/data
import gread/evolver
import gread/grammar
import gread/tableau
import gread/decompile

export stat
export lptabz
export lunacy
export lrucache

const
  greadSemanticErrorsAreFatal {.booldefine.} = false
  greadLuaCacheSize {.intdefine.} = 2048

type
  Fennel* = ref object
    core*: Option[int]
    vm*: PState
    runs*: uint
    nans*: FennelStat
    errors*: FennelStat
    runtime*: FennelStat
    aslua: GreadCache[Hash, string]

  FennelStat* = MovingStat[float32]

  Fun* = Function[Fennel]

  FProg* = Program[Fennel]
  FPop* = Population[Fennel]
  FEvo* = Evolver[Fennel, LuaValue]

  Locals* = SymbolSet[Fennel, LuaValue]

func isValid*(score: LuaValue): bool {.inline.} =
  unlikely score.kind != TInvalid

func strength*(score: LuaValue): float =
  if likely score.kind == TNumber:
    score.toFloat
  else:
    1.0
    #raise Defect.newException "only numbers are strong enough"

proc `$`*[T: Fennel](n: AstNode[T]): string =
  ## rendering fennel ast kinds
  result = $(FennelNodeKind n.kind) & "." & $n.operand

proc serialize*[S](output: var S; input: LuaValue) =
  ## serialize a LuaValue; used internally by frosty
  let js = input.toJson
  serialize(output, js[])

proc deserialize*[S](input: var S; output: var LuaValue) =
  ## deserialize a LuaValue; used internally by frosty
  var js: JsonNode
  new js
  deserialize(input, js[])
  output = js.toLuaValue

proc memoryGraphSize[T](thing: T): int =
  freeze(thing).len

proc asTable*[T](locals: SymbolSet[Fennel, LuaValue]): LPTab[string, T] =
  ## given locals, select values of the given type into a table
  init(result, initialSize = locals.len)
  for point in locals.items:
    template name: string = point.name
    template value: LuaValue = point.value
    when T is SomeFloat:
      if value.kind == TNumber:
        result[name] = value.toFloat
    elif T is SomeInteger:
      if value.kind == TNumber:
        result[name] = value.toInteger
    elif T is string:
      if value.kind == TString:
        result[name] = value.strung
    elif T is bool:
      if value.kind == TBoolean:
        result[name] = value.truthy

proc initLocals*[T, V](values: openArray[DataPoint[T, V]]): SymbolSet[T, V] =
  initSymbolSet[T, V](values)

proc initLocals*[V](values: openArray[(string, V)]): SymbolSet[Fennel, V] =
  ## convert an openArray of (name, value) pairs into Locals
  ## suitable for evaluate()
  initSymbolSet[Fennel, V](values)

proc clearStats*(fnl: Fennel) =
  ## reset the MovingStat values in the Fennel object
  clear fnl.nans
  clear fnl.errors
  #clear fnl.runtime
  when false:
    let began = getTime()
    fnl.vm.checkLua fnl.vm.gc(GcCollect, 0):
      discard
    notice fmt"collected in {(getTime() - began).inMilliseconds} µs"

const
  #[
    (global randp
      (fn []
        (if (= 1 (math.random 1))
          1
          nil
        )
      )
    )
    (global rand
      (fn [n] (math.random n))
    )

  ]#
  shims = """
    (global tgetN
      (fn [t i]
        (let [x (?. t i)]
          (if (= x nil)
            0.0
            x
          )
        )
      )
    )
  """

proc newFennel*(core = none int): Fennel =
  ## reset a Fennel instance and prepare it for running programs
  result = Fennel(vm: newState(), core: core)
  initGreadCache(result.aslua, initialSize=greadLuaCacheSize)
  clearStats result

  # setup the lua vm with the fennel compiler and any shims
  result.vm.openLibs()
  result.vm.checkLua result.vm.doString """fennel = require("fennel")""":
    let sham =
      """fennel.eval([==[$#]==], {compilerEnv=_G})""" % shims
    result.vm.checkLua result.vm.doString sham.cstring:
      discard

proc clearCache*(fnl: Fennel) {.deprecated.} =
  ## clear the execution cache of the Fennel instance;
  ## caching has been entirely lifted into the Evolver
  discard

proc fun*(s: string; arity = 0; args = arity..int.high): Fun =
  Fun(ident: s, arity: max(arity, args.a), args: args)

proc term*(value: float): Terminal =
  Terminal(kind: Float, floatVal: value)
proc term*(value: bool): Terminal =
  Terminal(kind: Boolean, boolVal: value)
proc str*(value: string): Terminal =
  Terminal(kind: String, strVal: value)
proc sym*(value: string): Terminal =
  Terminal(kind: Symbol, name: value)
proc term*(value: int): Terminal =
  Terminal(kind: Integer, intVal: value)

proc pushGlobal*(vm: PState; name: string; value: Terminal) =
  ## push a name/value pair into the vm as a global
  case value.kind
  of Float:
    vm.pushNumber value.floatVal
  of Boolean:
    vm.pushNumber value.boolVal.float
  of String:
    vm.pushString value.strVal.cstring
  of Integer:
    vm.pushNumber value.intVal.float
  else:
    raise ValueError.newException "unsupported term"
  vm.setGlobal name

proc hash(p: FProg; locals: Locals): Hash {.deprecated.} =
  ## produce a unique value according to program and training input
  var h: Hash = 0
  h = h !& p.hash
  h = h !& locals.hash
  result = !$h

converter toInt16(n: FennelNodeKind): int16 {.inline.} = int16 n

template fnk(n: typed): FennelNodeKind = FennelNodeKind n

func isParent*(n: AstNode[Fennel]): bool {.inline.} =
  fennelAllowedSubnodes[fnk n.kind] != {}

func isSymbol*(n: AstNode[Fennel]): bool {.inline.} =
  fnk(n.kind) == fennelSymbol

func isStringLit*(n: AstNode[Fennel]): bool {.inline.} =
  fnk(n.kind) == fennelString

func isNumberLit*(n: AstNode[Fennel]): bool {.inline.} =
  fnk(n.kind) in {fennelNumber, fennelBoolean}

func isEmpty*(n: AstNode[Fennel]): bool {.inline.} =
  fnk(n.kind) == fennelNil

proc render[T](a: Ast[T]; n: AstNode[T]; i = 0): string =
  ## render a fennel node from Ast `a`
  if n.isParent:
    case fnk(n.kind)
    of fennelMultiSymbol:
      var syms: seq[string]
      for sym in children(a, i):
        syms.add sym.name(0)
      syms.join(".")
    of fennelProgram:
      ""
    else:
      $n
  elif n.isSymbol:
    a.stringOp(n)
  elif n.isStringLit:
    escapeJson(a.stringOp(n), result)
    result
  elif fnk(n.kind) == fennelBoolean:
    $(bool a.numberOp(n))
  elif fnk(n.kind) in fennelTokenKinds:
    strRepr fnk(n.kind)
  elif n.isNumberLit:
    $cast[BiggestFloat](a.numberOp(n))
  else:
    raise Defect.newException:
      "unimpl node kind: $# ($#)" % [ strRepr(fnk n.kind), $n.kind ]

proc render*(a: Ast[Fennel]): string =
  ## render fennel ast in a form that can be compiled
  var i = 0
  var s = newSeqOfCap[tuple[index: int, ending: string]](a.len)
  template stripSpace {.dirty.} =
    if result.len > 0 and result[^1] in {' '}:
      setLen(result, result.high)
  template ensureSpace {.dirty.} =
    if result.len > 0 and result[^1] notin {' '}:
      result.add ' '
  template opener(x, y: string) {.dirty.} =
    ensureSpace()
    result.add x
    s.add (i+sizeOfSubtree(a, i), y)
    inc i
  template closer() {.dirty.} =
    while s.len > 0 and s[^1].index <= i:
      ensureSpace()
      result.add (pop s).ending
  while i <= a.high:
    case a[i].kind
    of fennelProgram:
      inc i                            # program is merely a semantic
    of fennelSequentialTable:
      opener("[", "]")
    of fennelList:
      opener("(", ")")
    elif a[i].isParent:
      ensureSpace()
      result.add a.render(a[i], i)     # probably just a multi.symbol
      inc(i, sizeOfSubtree(a, i))      # skip the parent's subtree
    else:
      ensureSpace()
      result.add a.render(a[i], i)     # rendering an individual node
      inc i
    closer()
  stripSpace()

proc compileFennel(vm: PState; source: string): string =
  vm.pushGlobal("result", term false)
  let fennel = """
    result = fennel.compileString([==[$#]==], {compilerEnv=_G})
  """ % [ source ]
  try:
    vm.checkLua vm.doString fennel.cstring:
      vm.getGlobal "result"
      result = vm.popStack.value.strung
  except LuaError as e:
    when greadSemanticErrorsAreFatal:
      error e.name, ": ", e.msg
      writeStackTrace()
      raise
  except Exception as e:
    error e.name, ": ", e.msg
    writeStackTrace()
    raise

proc toLua(fnl: Fennel; p: FProg): string =
  if p.hash in fnl.aslua:
    result = fnl.aslua[p.hash]
  else:
    result = compileFennel(fnl.vm, $p)
    memoryAudit "lua cache shenanigans":
      fnl.aslua[p.hash] = result

proc evaluateLua(vm: PState; s: string; locals: Locals): LuaStack =
  ## compile and evaluate the program as fennel; the result of
  ## the expression is assigned to the variable `result`.
  memoryAudit "evaluateLua all-in":
    for point in locals.items:
      discard vm.push point.value
      vm.setGlobal point.name.cstring
    try:
      vm.checkLua loadString(vm, s.cstring):
        vm.checkLua pcall(vm, 0, MultRet, 0):
          result = popStack vm
    except LuaError as e:
      when greadSemanticErrorsAreFatal:
        error e.name, ": ", e.msg
        writeStackTrace()
        raise
    except Exception as e:
      error e.name, ": ", e.msg
      writeStackTrace()
      raise

proc evaluate(vm: PState; s: string; locals: Locals): LuaStack =
  ## compile and evaluate the program as fennel; the result of
  ## the expression is assigned to the variable `result`.
  vm.pushGlobal("result", term 0.0)
  for point in locals.items:
    discard vm.push point.value
    vm.setGlobal point.name.cstring
  let fennel = """
    result = fennel.eval([==[$#]==], {compilerEnv=_G})
  """ % [ s ]
  try:
    vm.checkLua vm.doString fennel.cstring:
      vm.getGlobal "result"
      result = popStack vm
  except LuaError as e:
    when greadSemanticErrorsAreFatal:
      error e.name, ": ", e.msg
      writeStackTrace()
      raise
  except Exception as e:
    error e.name, ": ", e.msg
    writeStackTrace()
    raise

proc evaluate*(fnl: Fennel; p: FProg; locals: Locals): LuaValue =
  # prepare to run the vm
  result = LuaValue(kind: TInvalid)
  inc fnl.runs
  try:
    # pass the program and the training inputs
    when defined(greadNoFennelCache):
      let began = getMonoTime()
      let stack = evaluate(fnl.vm, $p, locals)
      fnl.runtime.push (getMonoTime() - began).inNanoseconds.float
    else:
      let source = fnl.toLua(p)
      let began = getMonoTime()
      let stack = evaluateLua(fnl.vm, source, locals)
      fnl.runtime.push (getMonoTime() - began).inNanoseconds.float
    fnl.errors.push 0.0
    # score a resultant value if one was produced
    if not stack.isNil:
      result = stack.value
  except LuaError as e:
    when greadSemanticErrorsAreFatal:
      debugEcho $p
      debugEcho e.msg
      quit 1
    else:
      discard e
    fnl.errors.push 1.0

  # any failure to produce a scorable value
  fnl.nans.push:
    if result.isValid:
      0.0
    else:
      p.zombie = true
      1.0

proc injectLocals*(p: FProg; locals: Locals): string =
  result = "(let ["
  for point in locals.items:
    result.add point.name
    result.add " "
    result.add $point.value
    result.add " "
  result.add "] "
  result.add $p
  result.add ")"

proc dumpScore*(fnl: Fennel; p: FProg) =
  var s = fmt"{p.score} {p.core}/{p.generation}[{p.len}]: "
  s.add $p
  checkpoint s

proc dumpPerformance*(fnl: Fennel; p: FProg; training: seq[Locals];
                      samples = 8) =
  ## dumps the performance of a program across `samples` training inputs;
  ## the stddev and sum of squares are provided
  if not p.isNil:
    var results = newSeqOfCap[float](training.len)
    for index, value in training.pairs:
      let s = evaluate(fnl, p, value)
      if index < samples or index == training.high:
        checkpoint "ideal: ", $value, "-> gp: ", s
      if s.isValid and s.kind == TNumber:
        results.add s.toFloat
    if results.len > 0:
      checkpoint "  stddev:", ff stddev(results)
      checkpoint "      ss: ", ff ss(results)
    fnl.dumpScore p

proc dumpPerformance*(fnl: Fennel; p: FProg; training: seq[(Locals, LuaValue)];
                      samples = 8) =
  ## as in the prior overload, but consumes training data with associated
  ## ideal result values for each set of symbolic inputs; correlation is
  ## additionally provided
  if not p.isNil:
    var results: seq[float]
    var ideals: seq[float]
    var deltas: seq[float]
    for index, value in training.pairs:
      let s = evaluate(fnl, p, value[0])
      var sf, vf: float
      if s.isValid and s.kind == TNumber:
        sf = s.toFloat
        vf = value[1].toFloat
        results.add sf
        ideals.add vf
        deltas.add abs(vf - sf)
      else:
        break
      if 0 == index mod samples or index == training.high:
        let delta = abs(vf - sf)
        if delta in [0.0, -0.0]:
          checkpoint fmt"{vf:>7.2f}"
        else:
          checkpoint fmt"ideal: {vf:>7.2f} -> gp: {sf:>7.2f}     -> {sum(deltas):>7.2f}    {index}"
    if results.len > 0:
      checkpoint "stddev:", stddev(results),
                 "corr:", correlation(results, ideals).percent,
                 "ss:", ss(results, ideals),
                 "of ideal:", (sum(results) / sum(ideals)).percent
    fnl.dumpScore p

proc dumpStats*(evo: Evolver; evoTime: Time) =
  ## a threadsafe echo of some statistics regarding the vm and population
  var fnl = evo.platform
  var pop = evo.population
  template genTime: FennelStat = evo.generationTime
  var m = pop.metrics
  let threaded = when compileOption"threads": $getThreadId() else: "-"
  if evo.fittest.isSome:
    paintFittest(m, get(evo.fittest))
    fnl.dumpScore get(evo.fittest)
  elif evo.generation > 1000:
    raise

  var dumb = m.lengths.variance.int  # work around nim bug
  # program cache usage: {(m.caches.mean / evo.dataset.len.float).percent}
  m.generation = evo.generation
  let age = int(m.generation.int.float - m.ages.mean)
  let totalTime = getTime() - evoTime
  let totalMs = totalTime.inMilliseconds.float
  when false:
    if m.generation.int == 0:
      raise ValueError.newException "no generations yet"
    if evo.tableau.maxGenerations == 0:
      raise ValueError.newException "no generation limit"
    if m.size == 0:
      raise ValueError.newException "empty population"
  checkpoint fmt"""
               core and thread: {m.core}/{threaded} -- {evo.name}
                  dataset size: {evo.dataset.len}
          virtual machine runs: {fnl.runs} (never reset)
         lua compilation cache: {fnl.aslua.len}
            average vm runtime: {fnl.runtime.mean / 1_000_000.0:>8.4f} ms
         total population size: {m.size}
          validity rate in pop: {m.validity.mean.percent} <= 100%
            average age in pop: {age}
            age vs generations: {percent(age.float / m.generation.int.float)} >= 0%
           average valid score: {ff m.scores.mean}
          greatest of all time: {m.bestScore}
           evolver cache count: {evo.cacheSize}
           evolver cache usage: {evo.cacheUsage.percent} >= 0%
          average program size: {m.lengths.mean.int}
         program size variance: {dumb}
          size of best program: {m.bestSize}
         parsimony coefficient: {ff m.parsimony}
            insufficiency rate: {fnl.nans.mean.percent} >= 0%
           semantic error rate: {fnl.errors.mean.percent} >= 0%
             foreign influence: {m.usurper}
              immigration rate: {(m.immigrants.float / m.size.float).percent}
          novel invention rate: {(m.inventions.float / m.size.float).percent}
          mapping failure rate: {evo.shortGenome.mean.percent}
               best generation: {m.bestGen}
             total generations: {m.generation} / {evo.tableau.maxGenerations}
        vm runs per generation: {ff(fnl.runs.float / m.generation.float)}
             invention recency: {m.staleness.percent} <= 100%
               generation time: {ff genTime.mean} ms
        generations per second: {ff(1000.0 * m.generation.float / totalMs)}
                evolution time: {ff(totalMs / 1000.0)} sec
  """
  clearStats fnl

proc programNode*[T: Fennel](a: var Ast[T]): AstNode[T] =
  ## create a head node for the program
  AstNode[T](kind: fennelProgram)

proc emptyNode*[T: Fennel](a: var Ast[T]): AstNode[T] =
  ## create an "empty" node suitable as a placeholder
  AstNode[T](kind: fennelNil)

proc terminalNode*[T: Fennel](a: var Ast[T]; term: Terminal): AstNode[T] =
  ## convert a terminal into an ast node
  case term.kind
  of Token:
    tokenNode(a, term.token, text = term.text)
  of None:
    AstNode[T](kind: fennelNil)
  of Symbol:
    AstNode[T](kind: fennelSymbol, operand: a.learnString term.name)
  of String:
    AstNode[T](kind: fennelString, operand: a.learnString term.strVal)
  of Float:
    AstNode[T](kind: fennelNumber,
               operand: a.learnNumber term.floatVal)
  of Integer:
    AstNode[T](kind: fennelNumber,  # transcode it to a float...
               operand: a.learnNumber term.intVal.float)
  of Boolean:
    AstNode[T](kind: fennelBoolean,
               operand: a.learnNumber term.boolVal)

proc composeCall*[T: Fennel](fun: Function[T]): Ast[T] =
  ## create a call of the given function
  result.nodes.add:
    terminalNode(result, Terminal(kind: Token, token: int16 fennelList))
  result =
    result.append(Terminal(kind: Symbol, name: fun.ident), parent = 0)

proc isFunctionSymbol*[T: Fennel](a: Ast[T]; index: int): bool {.deprecated: "nonsensical".} =
  if index > 0 and index < a.high:
    if not a[index].isParent:
      result = a[index].isSymbol and a[index-1].kind == fennelList

proc toAst[T: Fennel](node: TsFennelNode; s: string): Ast[T] =
  case node.kind
  of fennelSymbol:
    result = result.append Terminal(kind: Symbol, name: s[node])
  of fennelList, fennelMultiSymbol, fennelProgram, fennelSequentialTable:
    result = result.append Terminal(kind: Token, token: node.kind)
    for item in node.items:
      # create each child and add them to the parent
      result = result.append(toAst[T](item, s), parent = 0)
  of fennelComment:
    discard
  of fennelFalseTok:
    result = result.append: term false
  of fennelTrueTok:
    result = result.append: term true
  of fennelBoolean:
    result = result.append: term parseBool(s[node])
  of fennelNumber:
    result = result.append: term parseFloat(s[node])
  else:
    raise Defect.newException "unimplemented node: " & $node.kind

proc newFennelProgram*(s: string): Program[Fennel] =
  ## working around cps `()` operator shenanigans
  let tree = parseTsFennelString s
  if tree.kind != fennelProgram:
    raise ValueError.newException "expected a program; got " & $tree.kind
  else:
    result = newProgram toAst[Fennel](tree, s)

when compileOption"threads":
  import pkg/loony

  const hasSuru = compiles do: import suru
  when hasSuru:
    import suru
  import gread/cluster
  import gread/generation

  proc noop(c: C): C {.cpsMagic.} = c

  proc worker*(args: Work[Fennel, LuaValue]) {.cps: C.} =
    let fnl = newFennel(core = args.core)
    var evo: Evolver[Fennel, LuaValue]
    let rng =
      if args.rng.isSome:
        get args.rng
      else:
        initRand()
    initEvolver(evo, fnl, args.tableau, rng = rng)
    evo.name = args.name
    evo.strength = args.strength
    evo.grammar = args.grammar
    evo.operators = args.operators
    evo.dataset = args.dataset
    evo.core = fnl.core
    evo.fitone = args.fitone
    evo.fitmany = args.fitmany
    evo.population =
      if args.population.isNil:
        evo.randomPop()
      else:
        args.population
    evo.population.resetMetrics()
    evo.population.toggleParsimony(UseParsimony in evo.tableau)

    var evoTime = getTime()
    # fittest -> finest due to nim bug
    var finest: Option[Program[Fennel]]
    while evo.generation <= evo.tableau.maxGenerations:
      noop() # give other evolvers a chance

      search(args, evo.population)   # fresh meat from other threads

      memoryAudit "sharing a random member":
        let stale = randomMember(evo.population, evo.rng)
        share(args, stale.program)

      # share any new winner
      if evo.fittest.isSome:
        if finest.isNone or get(finest) != get(evo.fittest):
          finest = evo.fittest
          # NOTE: clone the finest so that we don't have to
          #       worry about the reference counter across threads
          forceShare(args, clone get(finest))

      memoryAudit "generational iteration":
        for discovery in evo.generation():
          discard

      if args.stats > 0:
        if evo.generation mod args.stats == 0:
          dumpStats(evo, evoTime)
          if defined(debug) and evo.generation > 10_000:
            debug fmt"memory consumption for evolver: {memoryGraphSize(evo)}"
            debug fmt"memory consumption for aslua: {memoryGraphSize(evo.platform.aslua)}"
            when defined(greadLargeCache):
              when defined(greadSlowTable):
                debug fmt"aslua (table) capacity: {rightSize(len evo.platform.aslua)}"
              else:
                debug fmt"aslua (table) capacity: {getCap evo.platform.aslua}"
            else:
              debug fmt"aslua (lru) capacity: {capacity evo.platform.aslua}"
          clearStats evo

    while evo.population.len > 0:
      share(args, randomRemoval(evo.population, evo.rng))

    quit 0

  proc runner*(args: Work[Fennel, LuaValue]) {.cps: C.} =
    ## a continuation that maps a population against a dataset
    let fnl = newFennel(core = args.core)
    var evo: Evolver[Fennel, LuaValue]
    initEvolver(evo, fnl, args.tableau)
    if args.rng.isSome:
      evo.rng = get args.rng
    evo.name = args.name
    evo.strength = args.strength
    evo.grammar = args.grammar
    evo.operators = args.operators
    evo.dataset = args.dataset
    evo.core = fnl.core
    evo.fitone = args.fitone
    evo.fitmany = args.fitmany
    # just trying to do a sorta-correct thing here...
    if args.population.isNil:
      evo.population = newPopulation[Fennel]()
    else:
      evo.population = args.population
      evo.population.toggleParsimony(UseParsimony in evo.tableau)

    while true:
      noop() # give other evolvers a chance

      var transit = pop args.io.inputs
      if transit.isNil:
        # we're done
        break

      # XXX: not using newSeq due to nim call operator bug
      var results: seq[Option[LuaValue]]
      setLen(results, evo.dataset.len)

      transit.source = getThreadId()
      transit.core = args.core
      let score = evo.score(transit)
      if score.isSome:
        # this is a fitmany result; ie. a single float
        transit.score = strength(evo)(get score)
      else:
        transit.score = NaN
      for index in 0..evo.dataset.high:
        results[index] = evo.score(index, transit)
      push(args.results, EvalResult[Fennel, LuaValue](program: transit,
                                                      results: results))

  proc scorer*(args: Work[Fennel, LuaValue]) {.cps: C.} =
    ## a continuation that simply scores programs in the input
    let fnl = newFennel(core = args.core)
    var evo: Evolver[Fennel, LuaValue]
    initEvolver(evo, fnl, args.tableau)
    if args.rng.isSome:
      evo.rng = get args.rng
    evo.name = args.name
    evo.strength = args.strength
    evo.grammar = args.grammar
    evo.operators = args.operators
    evo.dataset = args.dataset
    evo.core = fnl.core
    evo.fitone = args.fitone
    evo.fitmany = args.fitmany
    # just trying to do a sorta-correct thing here...
    if args.population.isNil:
      evo.population = newPopulation[Fennel]()
    else:
      evo.population = args.population
      evo.population.toggleParsimony(UseParsimony in evo.tableau)

    while true:
      noop() # give other evolvers a chance

      var transit = pop args.io.inputs
      if transit.isNil:
        # we're done
        break

      let s = evo.score(transit)
      if s.isSome:
        transit.score = strength(evo)(get s)
      else:
        transit.score = NaN
      transit.source = getThreadId()
      transit.core = args.core
      push(args.io.outputs, transit)

  template maybeProgressBar(iter: typed; body: untyped): untyped =
    when hasSuru:
      for it in suru(toSeq iter):
        body
    else:
      for it in iter:
        body

  proc threadedScore*[T: Fennel, V: LuaValue](args: var Work[T, V]; population: Population[T]): Population[T] =
    ## score programs in the population using multiple threads
    let clump = newCluster[T, V]()
    clump.redress args

    let (inputs, outputs) = clump.programQueues()
    for p in population.items:
      push(inputs, p)

    for core in 1..processors:
      args.rng = some initRand()
      clump.boot(whelp scorer(args), args.core)
      clump.redress args

    result = newPopulation[T](population.len)
    maybeProgressBar(1..population.len):
      while true:
        let p = pop outputs
        if p.isNil:
          sleep 5
        else:
          result.add p
          break

  proc threadedEvaluate*[T: Fennel, V: LuaValue](args: var Work[T, V]; population: Population[T]): LPTab[Program[T], seq[Option[V]]] =
    ## evaluate programs in the population using multiple threads
    init result
    let clump = newCluster[T, V]()
    clump.redress args

    let (inputs, outputs) = clump.programQueues()
    for p in population.items:
      push(inputs, p)

    for core in 1..processors:
      args.rng = some initRand()
      clump.boot(whelp runner(args), args.core)
      clump.redress args

    maybeProgressBar(1..population.len):
      while true:
        let eval = pop args.results
        if eval.isNil:
          sleep 5
        else:
          result[eval.program] = eval.results
          break

proc parseToken*[T: Fennel](s: string): FennelNodeKind =
  case s
  of "true":          fennelFalseTok
  of "false":         fennelTrueTok
  of "(":             fennelLParTok
  of ")":             fennelRParTok
  of "[":             fennelLBrackTok
  of "]":             fennelRBrackTok
  of ",":             fennelCommaTok
  of ".":             fennelDotTok
  of ":":             fennelColonTok
  of "or":            fennelOrTok
  of "nil":           fennelNil
  of "?":             fennelQuestionTok
  of "#":             fennelHashTok
  of "&":             fennelAmpersandTok
  of "&as":           fennelAmpersandasTok
  else:
    raise ValueError.newException "unsupported token: `$#`" % [ s ]

proc parseFennelToken*(s: string): int16 =
  ## generic-free fennel token parser for use in grammars
  parseToken[Fennel](s).int16

proc initFennelGrammar*(gram: var Grammar; syntax: string) =
  mixin initGrammar
  initGrammar(gram, parseFennelToken, syntax)

proc decompiler*[T: Fennel, G: LuaValue](d: var T; tableau: Tableau; gram: Grammar;
                                         source: string; rng: Rand = randState()): Evolver[T, G] =
  let program = newFennelProgram(source)
  if $program != source:
    raise Defect.newException fmt"code `{source}` parsed as `{program}`"
  #let codeAsCharacters = toSeq(repr program.ast)
  #let codeAsCharacters = toSeq(freeze program.ast)
  let codeAsCharacters = source.toSeq

  proc strong(score: LuaValue): float =
    -jaccard(toSeq $score, codeAsCharacters)

  proc fitter(d: T; data: SymbolSet[T, G]; p: Program[T]): Option[G] =
    #result = some (repr p.ast).toLuaValue
    #result = some (freeze p.ast).toLuaValue
    result = some ($p).toLuaValue

  proc fitthem(d: T; iter: iterator(): (ptr SymbolSet[T, G], ptr G);
               p: Program[T]): Option[G] =
    for symbols, s in iter():
      #return some (repr p.ast).toLuaValue
      #return some (freeze p.ast).toLuaValue
      return some ($p).toLuaValue

  var evo: Evolver[T, G]
  var tab = tableau
  tab.flags.excl UseParsimony
  initEvolver(evo, d, tab, rng)
  evo.operators = {
    geCrossover[T, G]:     2.0,
    geMutation[T, G]:      4.0,
    randomCrossover[T, G]: 1.0,
  }
  evo.strength = strong
  evo.grammar = gram
  evo.fitone = fitter
  evo.fitmany = fitthem
  evo.dataset = @[initSymbolSet[T, G]([("source", source.toLuaValue)])]
  evo.population = evo.randomPop()
  result = evo

proc del*[T: Fennel, V: LuaValue](evo: var Evolver[T, V]; p: Program[T]) =
  ## remove a program from the evolver; currently used to drop cache entries
  evolver.del(evo, p)
  try:
    del(evo.platform.aslua, p.hash)
  except KeyError:
    discard
