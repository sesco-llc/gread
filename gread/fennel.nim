import std/json
import std/times
import std/strformat
import std/sequtils
import std/math
import std/hashes
import std/options
import std/strutils

import pkg/lunacy except Integer
import pkg/adix/lptabz
import pkg/adix/stat
import pkg/balls
import pkg/cps
import pkg/frosty/streams as brrr
import pkg/htsparse/fennel/fennel_core_only as parsefen

import gread/spec
import gread/ast
import gread/population
import gread/programs
import gread/maths except variance
import gread/data
import gread/evolver

export stat
export lptabz
export lunacy

const
  semanticErrorsAreFatal = true
  initialCacheSize = 32*1024

type
  Fennel* = ref object
    core*: Option[int]
    vm*: PState
    runs*: uint
    cache: LPTab[Hash, Score]
    nans*: FennelStat
    errors*: FennelStat
    hits*: FennelStat
    runtime*: FennelStat

  FennelStat* = MovingStat[float32]

  Term* = Terminal[Fennel]
  Fun* = Function[Fennel]

  FProg* = Program[Fennel]
  FPop* = Population[Fennel]
  FEvo* = Evolver[Fennel, LuaValue]

  Locals* = SymbolSet[Fennel, LuaValue]

  FenFit = proc(locals: Locals; ideal: LuaValue): Score

proc `$`*[T: Fennel](n: AstNode[T]): string =
  ## rendering fennel ast kinds
  result = $(FennelNodeKind n.kind) & "." & $n.operand
  if n.flags != {}:
    result.add "/" & $n.flags

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
  clear fnl.hits
  clear fnl.runtime
  when false:
    let began = getTime()
    fnl.vm.checkLua fnl.vm.gc(GcCollect, 0):
      discard
    echo "collected in ", (getTime() - began).inMilliseconds, " µs"

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
  init(result.cache, initialSize = initialCacheSize)
  clearStats result

  # setup the lua vm with the fennel compiler and any shims
  result.vm.openLibs()
  result.vm.checkLua result.vm.doString """fennel = require("fennel")""":
    let sham =
      """fennel.eval([==[$#]==], {compilerEnv=_G})""" % shims
    result.vm.checkLua result.vm.doString sham.cstring:
      discard

proc clearCache*(fnl: Fennel) =
  ## clear the execution cache of the Fennel instance
  clear fnl.cache
  clear fnl.hits

proc fun*(s: string; arity = 0; args = arity..int.high): Fun =
  Fun(ident: s, arity: max(arity, args.a), args: args)

proc term*(value: float): Term =
  Term(kind: Float, floatVal: value)
proc term*(value: bool): Term =
  Term(kind: Boolean, boolVal: value)
proc str*(value: string): Term =
  Term(kind: String, strVal: value)
proc sym*(value: string): Term =
  Term(kind: Symbol, name: value)
proc term*(value: int): Term =
  Term(kind: Integer, intVal: value)

proc pushGlobal*(vm: PState; name: string; value: Term) =
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

proc hash(p: FProg; locals: Locals): Hash =
  ## produce a unique value according to program and training input
  var h: Hash = 0
  h = h !& p.hash
  h = h !& locals.hash
  result = !$h

converter toFennelNodeKind*(n: int16): FennelNodeKind {.inline.} = FennelNodeKind n
converter toInt16(n: FennelNodeKind): int16 {.inline.} = int16 n

func isParent*(n: AstNode[Fennel]): bool {.inline.} =
  fennelAllowedSubnodes[n.kind] != {}

func isSymbol*(n: AstNode[Fennel]): bool {.inline.} =
  n.kind == fennelSymbol

func isStringLit*(n: AstNode[Fennel]): bool {.inline.} =
  n.kind == fennelString

func isNumberLit*(n: AstNode[Fennel]): bool {.inline.} =
  n.kind in {fennelNumber, fennelBoolean}

func isEmpty*(n: AstNode[Fennel]): bool {.inline.} =
  n.kind == fennelNil

proc render[T](a: Ast[T]; n: AstNode[T]; i = 0): string =
  ## render a fennel node from Ast `a`
  if n.isParent:
    case n.kind
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
    a.strings[LitId n.operand]
  elif n.isStringLit:
    escapeJson(a.strings[LitId n.operand], result)
    result
  elif n.kind in fennelTokenKinds:
    strRepr n.kind
  elif n.isNumberLit:
    $cast[BiggestFloat](a.numbers[LitId n.operand])
  else:
    raise Defect.newException:
      "unimpl node kind: $# ($#)" % [ strRepr(n.kind), $n.kind ]

proc render*(a: Ast[Fennel]): string =
  ## render fennel ast in a form that can be compiled
  var i = 0
  var s = newSeqOfCap[int](a.len)
  template maybeAddSpace {.dirty.} =
    if result.len > 0 and result[^1] notin {'('}:
      result.add " "
  template closeParens {.dirty.} =
    while s.len > 0 and i >= s[^1]:
      result.add ")"
      discard pop s
  while i <= a.high:
    closeParens()
    maybeAddSpace()
    case a[i].kind
    of fennelProgram:
      inc i                            # program is merely a semantic
    of fennelList:
      result.add "("
      s.add i+sizeOfSubtree(a, i)      # pop when you get past index+size
      inc i
    elif a[i].isParent:
      result.add a.render(a[i], i)     # probably just a multi.symbol
      inc(i, sizeOfSubtree(a, i))      # skip the parent's subtree
    else:
      result.add a.render(a[i], i)     # rendering an individual node
      inc i
  closeParens()

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
  vm.checkLua vm.doString fennel.cstring:
    vm.getGlobal "result"
    result = popStack vm

proc evaluate*(fnl: Fennel; p: FProg; locals: Locals; fit: FenFit): Score =
  var h = hash(p, locals)
  try:
    # try to fetch the score from cache
    result = fnl.cache[h]
    fnl.hits.push 1.0
  except KeyError:
    # prepare to run the vm
    result = NaN
    fnl.hits.push 0.0
    inc fnl.runs
    try:
      # pass the program and the training inputs
      let began = getTime()
      let stack = evaluate(fnl.vm, $p, locals)
      fnl.runtime.push (getTime() - began).inMilliseconds.float
      fnl.errors.push 0.0
      # score a resultant value if one was produced
      if not stack.isNil:
        result = fit(locals, stack.value)
    except LuaError as e:
      when semanticErrorsAreFatal:
        debugEcho $p
        debugEcho e.msg
        quit 1
      else:
        discard e
      fnl.errors.push 1.0

    # any failure to produce a scorable value
    fnl.nans.push:
      if result.isNaN:
        p.zombie = true
        1.0
      else:
        # if it's valid, we'll cache it
        fnl.cache[h] = result
        0.0

proc dumpScore*(fnl: Fennel; p: FProg) =
  let code = $p
  var s =
    if p.score.isValid:
      $p.score
    else:
      "... "
  s.add fmt"[{p.len}] at #{p.generation} "
  if p.source != 0:
    s.add fmt"from {p.source} "
  s.add fmt"for {code}"
  checkpoint s

proc dumpPerformance*(fnl: Fennel; p: FProg; training: seq[Locals];
                      fenfit: FenFit; samples = 8) =
  ## dumps the performance of a program across `samples` training inputs;
  ## the stddev and sum of squares are provided
  if not p.isNil:
    var results = newSeqOfCap[float](training.len)
    for index, value in training.pairs:
      let s = evaluate(fnl, p, value, fenfit)
      if index < samples or index == training.high:
        checkpoint "ideal: ", $value, "-> gp: ", s
      if s.isValid:
        results.add s.float
    checkpoint "  stddev:", Score stddev(results)
    checkpoint "      ss: ", Score ss(results)
    fnl.dumpScore p

proc dumpPerformance*(fnl: Fennel; p: FProg; training: seq[(Locals, Score)];
                      fenfit: FenFit; samples = 8) =
  ## as in the prior overload, but consumes training data with associated
  ## ideal result values for each set of symbolic inputs; correlation is
  ## additionally provided
  if not p.isNil:
    var results: seq[float]
    var ideals: seq[float]
    var deltas: seq[float]
    for index, value in training.pairs:
      let s = evaluate(fnl, p, value[0], fenfit)
      if s.isValid:
        results.add s.float
        ideals.add value[1].float
        deltas.add abs(value[1].float - s.float)
      else:
        return
      if 0 == index mod samples or index == training.high:
        let delta = abs(value[1].float - s.float)
        if delta in [0.0, -0.0]:
          checkpoint fmt"{float(value[1]):>7.2f}"
        else:
          checkpoint fmt"ideal: {float(value[1]):>7.2f} -> gp: {float(s):>7.2f}     -> {sum(deltas):>7.2f}    {index}"
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
  let m = pop.metrics
  let threaded = when compileOption"threads": $getThreadId() else: "-"
  if not pop.fittest.isNil:
    fnl.dumpScore pop.fittest

  var dumb = m.lengths.variance.int  # work around nim bug
  checkpoint fmt"""
               core and thread: {m.core}/{threaded}
          virtual machine runs: {fnl.runs} (never reset)
            average vm runtime: {fnl.runtime.mean:>6.2f} ms
         total population size: {m.size}
            average age in pop: {int(m.generation.int.float - m.ages.mean)}
          validity rate in pop: {m.validity.mean.percent}
         program size variance: {dumb}
           average valid score: {Score m.scores.mean}
          greatest of all time: {m.bestScore}
          average program size: {m.lengths.mean.int}
          size of best program: {m.bestSize}
         parsimony coefficient: {Score m.parsimony}
            insufficiency rate: {fnl.nans.mean.percent}
           semantic error rate: {fnl.errors.mean.percent}
         recent lua cache hits: {int fnl.hits.sum}
            lua cache hit rate: {fnl.hits.mean.percent}
             lua vm cache size: {fnl.cache.len}
             foreign influence: {m.usurper}
              immigration rate: {(m.immigrants.float / m.size.float).percent}
          mapping failure rate: {evo.shortGenome.mean.percent}
               best generation: {m.bestGen}
             total generations: {m.generation}
             invention recency: {m.staleness.percent}
               generation time: {Score genTime.mean} ms
                evolution time: {(getTime() - evoTime).inSeconds} sec
  """
  clearStats fnl

proc programNode*[T: Fennel](a: var Ast[T]): AstNode[T] =
  ## create a head node for the program
  AstNode[T](kind: fennelProgram)

proc emptyNode*[T: Fennel](a: var Ast[T]): AstNode[T] =
  ## create an "empty" node suitable as a placeholder
  AstNode[T](kind: fennelNil)

proc terminalNode*[T: Fennel](a: var Ast[T]; term: Terminal[T]): AstNode[T] =
  ## convert a terminal into an ast node
  case term.kind
  of Token:
    tokenNode(a, term.token, text = term.text)
  of None:
    AstNode[T](kind: fennelNil)
  of Symbol:
    AstNode[T](kind: fennelSymbol, operand: a.strings.getOrIncl term.name)
  of String:
    AstNode[T](kind: fennelString, operand: a.strings.getOrIncl term.strVal)
  of Float:
    AstNode[T](kind: fennelNumber,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.floatVal))
  of Integer:
    AstNode[T](kind: fennelNumber,  # transcode it to a float...
               operand: a.numbers.getOrIncl cast[BiggestInt](float term.intVal))
  of Boolean:
    AstNode[T](kind: fennelBoolean,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.boolVal))

proc composeCall*[T: Fennel](fun: Function[T]): Ast[T] =
  ## create a call of the given function
  result.nodes.add:
    terminalNode(result, Terminal[T](kind: Token, token: int16 fennelList))
  result =
    result.append(Terminal[T](kind: Symbol, name: fun.ident), parent = 0)

proc isFunctionSymbol*[T: Fennel](a: Ast[T]; index: int): bool {.deprecated: "nonsensical".} =
  if index > 0 and index < a.high:
    if not a[index].isParent:
      result = a[index].isSymbol and a[index-1].kind == fennelList

proc toAst[T: Fennel](node: TsFennelNode; s: string): Ast[T] =
  case node.kind
  of fennelSymbol:
    result = result.append Terminal[T](kind: Symbol, name: s[node])
  of fennelList, fennelMultiSymbol, fennelProgram:
    result = result.append Terminal[T](kind: Token, token: node.kind)
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
  import gread/cluster
  import gread/generation

  proc noop(c: C): C {.cpsMagic.} = c

  proc worker*(args: Work[Fennel, LuaValue]) {.cps: C.} =
    let fnl = newFennel(core = args.core)
    var evo: Evolver[Fennel, LuaValue]
    initEvolver(evo, fnl, args.tableau)
    evo.grammar = args.grammar
    evo.operators = args.operators
    evo.dataset = args.dataset
    evo.core = fnl.core
    evo.fitone = args.fitone
    evo.fitmany = args.fitmany
    evo.population = evo.randomPop()

    var leader: Hash
    var evoTime = getTime()
    while evo.population.generations.int <= evo.tableau.maxGenerations:
      noop() # give other evolvers a chance

      for invalid in invalidPrograms(args):
        fnl.cache[invalid.hash] = Score NaN

      search(args, evo.population)   # fresh meat from other threads

      let fit = evo.fittest
      if fit.isSome:
        let fit = get fit
        if fit.hash != leader:
          leader = fit.hash
          share(args, fit)  # send it to other threads

      if evo.tableau.useParsimony:
        profile "parsimony":
          discard evo.population.parsimony

      let invention = evo.generation()

      if invention.isSome:
        let p = get invention
        if p.core.isNone and fnl.core.isSome:
          p.core = fnl.core

        if p.generation mod args.stats == 0:
          dumpStats(evo, evoTime)
          clearStats evo

        when false:
          if p.score.isNaN:
            negativeCache(args, p)
    quit 0

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
