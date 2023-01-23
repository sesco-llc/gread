import std/hashes
import std/packedsets
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

import pkg/adix/stat except Option
import pkg/balls
import pkg/cps
import pkg/frosty/streams as brrr
import pkg/grok/kute
import pkg/grok/resources
import pkg/htsparse/fennel/fennel_core_only as parsefen
import pkg/insideout
import pkg/lrucache
import pkg/lunacy except Integer
import pkg/lunacy/json as luajs

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
import gread/audit
import gread/genotype

export lunacy
export lrucache
export stat except Option  # nim bug workaround

const
  greadLuaCacheSize {.intdefine.} = 2048

type
  FennelObj = object
    core*: Option[int]
    vm*: PState
    runs*: uint
    nans*: FennelStat
    errors*: FennelStat
    runtime*: FennelStat
  Fennel* = ref FennelObj

  FennelStat* = MovingStat[float32, uint32]

  Fun* = Function[Fennel]

  FProg* = Program[Fennel]
  FPop* = Population[Fennel]
  FEvo* = Evolver[Fennel, LuaValue]

  Locals* = SymbolSet[Fennel, LuaValue]

proc `=destroy`(dest: var FennelObj) =
  if not dest.vm.isNil:
    close dest.vm
  for key, value in dest.fieldPairs:
    reset value

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

proc asTable*[T](locals: SymbolSet[Fennel, LuaValue]): GreadTable[string, T] =
  ## given locals, select values of the given type into a table
  initGreadTable(result, initialSize = locals.len)
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

proc setupVM(vm: PState) =
  # setup the lua vm with the fennel compiler and any shims
  vm.openLibs()
  vm.checkLua vm.doString """fennel = require("fennel")""":
    let sham =
      """fennel.eval([==[$#]==], {compilerEnv=_G})""" % shims
    vm.checkLua vm.doString sham.cstring:
      discard

proc newVM(): PState =
  result = newState()
  setupVM result

proc tidyVM*(fnl: Fennel) =
  let began = getMonoTime()
  when true:
    close fnl.vm
    fnl.vm = newVM()
    #echo fmt"reboot vm in {(getMonoTime() - began).inMilliseconds} ms"
  else:
    fnl.vm.checkLua fnl.vm.gc(GcCollect, 0):
      discard
    echo fmt"gccollect in {(getMonoTime() - began).inMilliseconds} ms"

proc clearStats*(fnl: Fennel) =
  ## reset the MovingStat values in the Fennel object
  clear fnl.nans
  clear fnl.errors
  clear fnl.runtime
  fnl.tidyVM()

proc newFennel*(core = none int): Fennel =
  ## reset a Fennel instance and prepare it for running programs
  result = Fennel(vm: newVM(), core: core)

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

proc composeMultiSymbol[T](a: Ast[T]; i = 0): string =
  var syms: seq[string]
  for sym in children(a, i):
    syms.add sym.name(0)
  result = syms.join(".")

proc composeStringLiteral[T](a: Ast[T]; n: AstNode[T]): string =
  escapeJson(a.stringOp(n), result)

proc render[T](a: Ast[T]; n: AstNode[T]; i = 0): string =
  ## render a fennel node from Ast `a`
  if n.isParent:
    case fnk(n.kind)
    of fennelMultiSymbol:
      result = composeMultiSymbol(a, i)
    of fennelProgram:
      result = ""
    else:
      result = $n
  elif n.isSymbol:
    result = a.stringOp(n)
  elif n.isStringLit:
    result = composeStringLiteral(a, n)
  elif fnk(n.kind) == fennelBoolean:
    var number: bool
    number = cast[bool](a.numberOp(n))
    result = $number
  elif fnk(n.kind) in fennelTokenKinds:
    result = strRepr fnk(n.kind)
  elif n.isNumberLit:
    var number: BiggestFloat
    number = cast[BiggestFloat](a.numberOp(n))
    result = $number
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
  compact result

proc `$`*(program: FProg): string =
  if Rendered in program.flags:
    programs.`$`(program)
  else:
    warn "had to render an immutable program"
    render program.ast

proc compileFennel(vm: PState; source: string): string =
  vm.pushGlobal("result", term false)
  let fennel = fmt"""
    result = fennel.compileString([==[{source}]==], {{compilerEnv=_G}})
  """
  let code: cstring = fennel
  try:
    vm.checkLua vm.doString code:
      vm.getGlobal "result"
      result = vm.popStack.value.strung
  except LuaError as e:
    when greadSemanticErrorsAreFatal:
      error e.name, ": ", e.msg
      writeStackTrace()
      raise
  except CatchableError as e:
    error e.name, ": ", e.msg
    writeStackTrace()
    raise

proc compileFennel*(fnl: Fennel; source: string): string =
  compileFennel(fnl.vm, source)

proc evaluateLua(vm: PState; s: string; locals: Locals): LuaStack =
  ## evaluate the string as lua and pop the stack for the result
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
  except CatchableError as e:
    error e.name, ": ", e.msg
    writeStackTrace()
    raise

proc evaluate(vm: PState; s: string; locals: Locals): LuaStack {.deprecated.} =
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
  except CatchableError as e:
    error e.name, ": ", e.msg
    writeStackTrace()
    raise

proc evaluateLua*(fnl: Fennel; code: string; locals: Locals): LuaValue =
  result = LuaValue(kind: TInvalid)
  inc fnl.runs
  if fnl.runs mod 50_000 == 0:
    fnl.tidyVM()
  try:
    # pass the program and the training inputs
    let began = getMonoTime()
    let stack = evaluateLua(fnl.vm, code, locals)
    fnl.runtime.push (getMonoTime() - began).inNanoseconds.float
    fnl.errors.push 0.0
    # score a resultant value if one was produced
    if not stack.isNil:
      result = stack.value
  except LuaError as e:
    when greadSemanticErrorsAreFatal:
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
      1.0

proc evaluate*(fnl: Fennel; p: var FProg; locals: Locals): LuaValue =
  var code = compileFennel(fnl.vm, $p)
  result = evaluateLua(fnl, code, locals)
  if result.kind == TInvalid:
    p.zombie = true

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

proc threadName*(core: CoreSpec): string =
  ## render the core with local thread-id `when compileOption"threads"`
  when compileOption"threads":
    result.add $getThreadId()
    result.add ":"
  result.add $core

proc dumpScore*(p: FProg) =
  var s = fmt"{p.score} {p.core}/{p.generation}[{p.genome.len}]: "
  s.add $p
  s.add " <"
  s.add $hash(p.genome)
  s.add ">"
  checkpoint s

proc dumpScore*(fnl: Fennel; p: FProg) {.deprecated: "use dumpScore/1".} =
  dumpScore p

proc getStats*(evo: Evolver; evoTime: Time): string =
  ## compose a report of some statistics regarding the vm and population
  let fnl = evo.platform
  template genTime: FennelStat = evo.generationTime
  var m: PopMetrics
  m.paintMetrics(evo)

  var dumb = m.lengths.variance.int  # work around nim bug
  # program cache usage: {(m.caches.mean / evo.dataset.len.float).percent}
  m.generation = evo.generation
  let age = int(m.generation.int.float - m.ages.mean)
  let totalTime = getTime() - evoTime
  let totalMs = totalTime.inMilliseconds.float
  let bestRuntime =
    if evo.fittest.isSome:
      fmt"{get(evo.fittest).runtime.mean / 1_000_000.0:>8.4f} ms"
    else:
      "nan"
  var threadStats: string
  var process: ProcessResources
  sample process
  threadStats.add "\n" & """
    voluntary context switches: {process.voluntaryContextSwitches}""".fmt
  threadStats.add "\n" & """
  involuntary context switches: {process.involuntaryContextSwitches}""".fmt
  threadStats.add "\n" & """
                   memory used: {Kute memoryUsed()} of {Kute memoryArena()}""".fmt
  threadStats.add "\n" & """
                process memory: {Kute process.maxResidentBytes} ({processors} threads)""".fmt
  when false:
    if m.generation.int == 0:
      raise ValueError.newException "no generations yet"
    if evo.tableau.maxGenerations == 0:
      raise ValueError.newException "no generation limit"
    if m.size == 0:
      raise ValueError.newException "empty population"
  result = fmt"""
               thread and core: {m.core.threadName} -- {evo.name}
                  dataset size: {evo.dataset.len}
          virtual machine runs: {fnl.runs} (never reset)
            average vm runtime: {fnl.runtime.mean / 1_000_000.0:>8.4f} ms
         total population size: {m.size}
          validity rate in pop: {m.validity.mean.percent} <= 100%
            average age in pop: {age}
            age vs generations: {percent(age.float / m.generation.int.float)} >= 0%
           average valid score: {Score m.scores.mean}
          greatest of all time: {m.bestScore}
           evolver cache count: {evo.cacheSize}
           evolver cache usage: {evo.cacheUsage.percent} >= 0%
          average program size: {m.lengths.mean.int}
         program size variance: {dumb}
          size of best program: {m.bestSize}
       runtime of best program: {bestRuntime}
         parsimony coefficient: {ff m.parsimony}
            insufficiency rate: {fnl.nans.mean.percent} >= 0%
           semantic error rate: {fnl.errors.mean.percent} >= 0%
             foreign influence: {m.usurper}
              total immigrants: {m.immigrants}
             invention recency: {m.staleness.percent} <= 100%
              total inventions: {m.inventions}
                leader changes: {m.leaders}
               zombies created: {m.zombies}
          mapping failure rate: {evo.shortGenome.mean.percent}
               best generation: {m.bestGen}
             total generations: {m.generation} / {evo.tableau.maxGenerations}
        vm runs per generation: {ff(fnl.runs.float / m.generation.float)}
               generation time: {ff genTime.mean} ms
        generations per second: {ff(1000.0 * m.generation.float / totalMs)}
                evolution time: {ff(totalMs / 1000.0)} sec"""
  result &= threadStats
  clearStats fnl

proc dumpStats*(evo: Evolver; evoTime: Time) =
  ## a threadsafe echo of some statistics regarding the vm and population
  checkpoint getStats(evo, evoTime) & "\n"

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

proc parseFennelString(str: string): TsFennelNode =
  let parser = newTsFennelParser()
  try:
    result = parseString(parser, str)
  finally:
    tsParserDelete parser.PtsParser

proc newFennelProgram*(s: string): Program[Fennel] =
  ## create a program from fennel source code
  let node = parseFennelString s
  try:
    if node.kind != fennelProgram:
      raise ValueError.newException "expected a program; got " & $node.kind
    else:
      result = newProgram toAst[Fennel](node, s)
  finally:
    tsTreeDelete node.TSNode.tree

when compileOption"threads":
  const hasSuru = compiles do: import suru
  when hasSuru:
    import suru
  import gread/cluster
  import gread/generation

  proc coop(c: C): C {.cpsMagic.} = c

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
    if args.population.isNil:
      evo.population = evo.randomPop()
    else:
      evo.population = args.population
      # this worker currently assumes that the lesser evil
      # is to score the entire population, as opposed to
      # simply spewing "fit" programs which are merely the
      # fittest found thus far among an existing population
      for program in evo.population.mitems:
        discard evo.paintScore(program, inPop=true)
      if UseParsimony in evo.tableau:
        evo.toggleParsimony(on)
      else:
        evo.toggleParsimony(off)
        evo.resetFittest()

    var evoTime = getTime()
    # fittest -> finest due to nim bug
    var finest: Option[Program[Fennel]]
    while evo.generation <= evo.tableau.maxGenerations:
      coop() # give other evolvers a chance

      if evo.rng.rand(1.0) < args.tableau.sharingRate:
        # receive messages from other members of the cluster
        var transport: ClusterTransport[Fennel, LuaValue]
        if tryRecv(args.io.input, transport):
          case transport.kind
          of ctControl:
            case transport.control
            of ckWorkerQuit:
              info "terminating on request"
              break
          else:
            for program in programs(transport):
              evo.add program
        else:
          # send programs to other members of the cluster
          let stale = randomMember(evo.population, evo.rng)
          shareInput(args, stale.program)

          # share any new winner
          if evo.fittest.isSome:
            assert FinestKnown in evo.fittest.get.flags
            if finest.isNone or get(finest) != get(evo.fittest):
              finest = evo.fittest
              assert FinestKnown in finest.get.flags
              shareOutput(args, get finest)

      for discovery in evo.generation():
        discard

      if args.stats > 0:
        if evo.generation mod args.stats == 0:
          evo.dumpStats(evoTime)
          evo.clearStats()

    when defined(greadShareEntirePopulationAtDeath):
      let shared = evo.population
      evo.population = nil
      push(args.io.output, shared)
    else:
      if evo.fittest.isSome:
        shareOutput(args, get evo.fittest)

    push(args.io.output, ckWorkerQuit)

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
      if UseParsimony in evo.tableau:
        evo.toggleParsimony(on)
      else:
        evo.toggleParsimony(off)
        evo.resetFittest()

    while true:
      coop() # give other evolvers a chance

      let transport = recv args.io.input
      if transport.isNil:
        # we're done
        break
      else:
        # XXX: not using newSeq due to nim call operator bug
        var results: seq[Option[LuaValue]]
        setLen(results, evo.dataset.len)

        case transport.kind
        of ctControl:
          case transport.control
          of ckWorkerQuit:
            info "terminating on request"
            break
          #else:
          #  warn "ignoring control: " & $transport.control
        else:
          for transit in programs(transport):
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
            let evaluated = EvalResult[Fennel, LuaValue](program: transit,
                                                         results: results)
            push(args.io.output, evaluated)

    push(args.io.output, ckWorkerQuit)

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
      if UseParsimony in evo.tableau:
        evo.toggleParsimony(on)
      else:
        evo.toggleParsimony(off)
        evo.resetFittest()

    while true:
      coop() # give other evolvers a chance

      let transport = pop args.io.input
      if transport.isNil:
        # we're done
        break
      else:
        case transport.kind
        of ctControl:
          case transport.control
          of ckWorkerQuit:
            info "terminating on request"
            break
          #else:
          #  warn "ignoring control: " & $transport.control
        else:
          for transit in programs(transport):
            let s = evo.score(transit)
            if s.isSome:
              transit.score = strength(evo)(get s)
            else:
              transit.score = NaN
            transit.source = getThreadId()
            transit.core = args.core
            push(args.io.output, transit)

    push(args.io.output, ckWorkerQuit)

  template maybeProgressBar(iter: typed; body: untyped): untyped =
    when hasSuru:
      for it in suru(toSeq iter):
        body
    else:
      for it in iter:
        body

  proc threadedScore*[T: Fennel, V: LuaValue](args: var Work[T, V]; population: Population[T]; cores = none int): Population[T] =
    ## score programs in the population using multiple threads
    if population.len == 0:
      return population
    let clump = newCluster[T, V]()
    clump.redress args

    let (inputs, outputs) = clump.programQueues()
    for p in population.items:
      push(inputs, p)

    let processors =
      if cores.isSome:
        get cores
      else:
        processors
    for core in 1..processors:
      args.rng = some initRand()
      clump.boot(whelp scorer(args))
      clump.redress args

    result = newPopulation[T](population.len)
    maybeProgressBar(1..population.len):
      while true:
        let transport = recv outputs
        case transport.kind
        of ctControl:
          case transport.control
          of ckWorkerQuit:
            debug "worker terminated"
          #else:
          #  warn "ignoring control: " & $transport.control
        else:
          for program in programs(transport):
            result.add program
          break

  proc threadedEvaluate*[T: Fennel, V: LuaValue](args: var Work[T, V]; population: Population[T]; cores = none int): GreadTable[Program[T], seq[Option[V]]] =
    ## evaluate programs in the population using multiple threads
    initGreadTable result
    let clump = newCluster[T, V]()
    clump.redress args

    let (inputs, outputs) = clump.programQueues()
    for p in population.items:
      push(inputs, p)

    let processors =
      if cores.isSome:
        get cores
      else:
        processors
    for core in 1..processors:
      args.rng = some initRand()
      clump.boot(whelp runner(args))
      clump.redress args

    maybeProgressBar(1..population.len):
      while true:
        let transport = recv args.io.output
        case transport.kind
        of ctControl:
          case transport.control
          of ckWorkerQuit:
            debug "worker terminated"
        of ctEvalResult:
          result[transport.result.program] = transport.result.results
          break
        else:
          warn "threadedEvaluate() ignoring " & $transport.control

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

  proc fitter(d: T; data: SymbolSet[T, G]; p: var Program[T]): Option[G] =
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
    geNoise1pt0[T, G]:     10.0,
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
