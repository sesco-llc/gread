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
import pkg/htsparse/lua/lua_core_only as parselua

import gread/spec
import gread/ast
import gread/population
import gread/programs
import gread/maths except variance
import gread/data
import gread/evolver
import gread/grammar

export stat
export lptabz
export lunacy

{.passL: "-lstdc++".}  # workaround buglet with htsparse(?)

const
  semanticErrorsAreFatal = false
  initialCacheSize = 32*1024

type
  Lua* = ref object
    core*: Option[int]
    vm*: PState
    runs*: uint
    nans*: LuaStat
    errors*: LuaStat
    runtime*: LuaStat

  LuaStat* = MovingStat[float32]

  Fun* = Function[Lua]

  LProg* = Program[Lua]
  LPop* = Population[Lua]
  LEvo* = Evolver[Lua, LuaValue]

  Locals* = SymbolSet[Lua, LuaValue]

proc isValid*(score: LuaValue): bool =
  score.kind != TInvalid

proc strength*(score: LuaValue): float =
  if score.kind == TNumber:
    score.toFloat
  else:
    raise Defect.newException "only numbers are strong enough"

proc `$`*[T: Lua](n: AstNode[T]): string =
  ## rendering lua ast kinds
  result = $(LuaNodeKind n.kind) & "." & $n.operand

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

proc asTable*[T](locals: SymbolSet[Lua, LuaValue]): LPTab[string, T] =
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

proc clearStats*(lua: Lua) =
  ## reset the MovingStat values in the Lua object
  clear lua.nans
  clear lua.errors
  clear lua.runtime
  when false:
    let began = getTime()
    lua.vm.checkLua lua.vm.gc(GcCollect, 0):
      discard
    echo "collected in ", (getTime() - began).inMilliseconds, " ??s"

proc newLua*(core = none int): Lua =
  ## reset a Lua instance and prepare it for running programs
  result = Lua(vm: newState(), core: core)
  clearStats result

  # setup the lua vm
  result.vm.openLibs()

proc clearCache*(lua: Lua) {.deprecated.} =
  ## clear the execution cache of the Lua instance;
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

proc hash(p: LProg; locals: Locals): Hash =
  ## produce a unique value according to program and training input
  var h: Hash = 0
  h = h !& p.hash
  h = h !& locals.hash
  result = !$h

converter toLuaNodeKind*(n: int16): LuaNodeKind {.inline.} = LuaNodeKind n
converter toInt16(n: LuaNodeKind): int16 {.inline.} = int16 n

func isParent*(n: AstNode[Lua]): bool {.inline.} =
  luaAllowedSubnodes[n.kind] != {}

const
  luaParents =
    block:
      var s: set[LuaNodeKind]
      for kind in LuanodeKind.items:
        if luaAllowedSubnodes[kind] != {}:
          s.incl kind
      s

func isSymbol*(n: AstNode[Lua]): bool {.inline.} =
  n.kind in {luaIdentifier, luaPropertyIdentifier}

func isStringLit*(n: AstNode[Lua]): bool {.inline.} =
  n.kind == luaString

func isNumberLit*(n: AstNode[Lua]): bool {.inline.} =
  n.kind in {luaNumber, luaFalse, luaTrue}

func isEmpty*(n: AstNode[Lua]): bool {.inline.} =
  n.kind == luaNil

proc render[T: Lua](a: Ast[T]; n: AstNode[T]; i = 0): string =
  ## render a lua node from Ast `a`
  proc renderedKids(a: Ast[Lua]; index: int): seq[string] =
    for child in a.children(index):
      result.add render(child, child[0])

  case n.kind.LuaNodeKind  # workaround nim compiler crash bug
  of luaProgram:
    ""
  of luaNil:     "nil"
  of luaFalse:   "false"
  of luaTrue:    "true"
  of luaIdentifier, luaPropertyIdentifier:
    a.strings[LitId n.operand]
  of luaNumber:
    $cast[BiggestFloat](a.numbers[LitId n.operand])
  of luaString:
    escapeJson(a.strings[LitId n.operand], result)
    result
  of luaTokenKinds:
    strRepr n.kind
  elif n.kind in {luaFieldExpression}:
    renderedKids(a, i).join("")
  elif n.kind in luaParents:
    renderedKids(a, i).join(" ")
  else:
    raise Defect.newException:
      "unimpl node kind: $# ($#)" % [ strRepr(n.kind), $n.kind ]

proc render*(a: Ast[Lua]): string =
  ## render lua ast in a form that can be compiled
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
    of luaProgram:
      inc i                            # program is merely a semantic
    of luaExpression:
      result.add "("
      s.add i+sizeOfSubtree(a, i)      # pop when you get past index+size
      inc i
    elif a[i].isParent:
      result.add a.render(a[i], i)     # append a render of the subtree
      inc(i, sizeOfSubtree(a, i))      # and then move past it
    else:
      result.add a.render(a[i], i)     # rendering an individual node
      inc i
  closeParens()

proc evaluate(vm: PState; s: string; locals: Locals): LuaStack =
  ## evaluate the lua program; the result of the expression is
  ## assigned to the variable `result`.
  for point in locals.items:
    discard vm.push point.value
    vm.setGlobal point.name.cstring
  vm.checkLua vm.doString s.cstring:
    result = popStack vm

proc evaluate*(lua: Lua; p: LProg; locals: Locals): LuaValue =
  # prepare to run the vm
  result = LuaValue(kind: TInvalid)
  inc lua.runs
  try:
    # pass the program and the training inputs
    let began = getTime()
    let stack = evaluate(lua.vm, $p, locals)
    lua.runtime.push (getTime() - began).inMilliseconds.float
    lua.errors.push 0.0
    # score a resultant value if one was produced
    if not stack.isNil:
      result = stack.value
  except LuaError as e:
    when semanticErrorsAreFatal:
      debugEcho $p
      debugEcho e.msg
      quit 1
    else:
      discard e
    lua.errors.push 1.0

  # any failure to produce a scorable value
  lua.nans.push:
    if result.isValid:
      0.0
    else:
      p.zombie = true
      1.0

proc dumpScore*(lua: Lua; p: LProg) =
  var s = fmt"{p.score}[{p.len}]: "
  s.add $p
  checkpoint s

proc dumpPerformance*(lua: Lua; p: LProg; training: seq[Locals];
                      samples = 8) =
  ## dumps the performance of a program across `samples` training inputs;
  ## the stddev and sum of squares are provided
  if not p.isNil:
    var results = newSeqOfCap[float](training.len)
    for index, value in training.pairs:
      let s = evaluate(lua, p, value)
      if index < samples or index == training.high:
        checkpoint $value, "->", s
      if s.isValid:
        results.add s.toFloat
    checkpoint "  stddev:", Score stddev(results)
    checkpoint "      ss: ", Score ss(results)
    lua.dumpScore p

proc dumpPerformance*(lua: Lua; p: LProg; training: seq[(Locals, LuaValue)];
                      samples = 8) =
  ## as in the prior overload, but consumes training data with associated
  ## ideal result values for each set of symbolic inputs; correlation is
  ## additionally provided
  if not p.isNil:
    var results: seq[float]
    var ideals: seq[float]
    for index, value in training.pairs:
      let s = evaluate(lua, p, value[0])
      if not s.isValid:
        return
      if 0 == index mod samples or index == training.high:
        let delta = s.float
        if delta in [0.0, -0.0]:
          checkpoint fmt"{float(value[1]):>7.2f}"
        else:
          checkpoint fmt"{float(value[1]):>7.2f} -> {delta:>7.2f}     -> {sum(results):>7.2f}    {index}"
      if s.isValid and s.kind == TNumber:
        results.add s.toFloat
        ideals.add abs(value[1].float)
    if results.len > 0:
      checkpoint "stddev:", stddev(results),
                 "corr:", correlation(results, ideals).percent,
                 "ss:", ss(results, ideals),
                 "of ideal:", (sum(results) / sum(ideals)).percent
    lua.dumpScore p

proc dumpStats*(evo: Evolver; evoTime: Time) =
  ## a threadsafe echo of some statistics regarding the vm and population
  var lua = evo.platform
  var pop = evo.population
  template genTime: LuaStat = evo.generationTime
  let m = pop.metrics
  let threaded = when compileOption"threads": $getThreadId() else: "-"
  if not pop.fittest.isNil:
    lua.dumpScore pop.fittest

  var dumb = m.lengths.variance.int  # work around nim bug
  # program cache usage: {(m.caches.mean / evo.dataset.len.float).percent}
  let age = int(m.generation.int.float - m.ages.mean)
  checkpoint fmt"""
               core and thread: {m.core}/{threaded} -- {evo.name}
                  dataset size: {evo.dataset.len}
          virtual machine runs: {lua.runs} (never reset)
            average vm runtime: {lua.runtime.mean:>6.2f} ms
         total population size: {m.size}
          validity rate in pop: {m.validity.mean.percent}
            average age in pop: {age}
            age vs generations: {percent(age.float / m.generation.int.float)}
           average valid score: {Score m.scores.mean}
          greatest of all time: {m.bestScore}
           evolver cache count: {evo.cacheSize}
           evolver cache usage: {evo.cacheUsage.percent}
          average program size: {m.lengths.mean.int}
         program size variance: {dumb}
          size of best program: {m.bestSize}
         parsimony coefficient: {Score m.parsimony}
            insufficiency rate: {lua.nans.mean.percent}
           semantic error rate: {lua.errors.mean.percent}
             foreign influence: {m.usurper}
              immigration rate: {(m.immigrants.float / m.size.float).percent}
               best generation: {m.bestGen}
             total generations: {m.generation}
             invention recency: {m.staleness.percent}
               generation time: {Score genTime.mean} ms
                evolution time: {(getTime() - evoTime).inSeconds} sec
  """
  clearStats lua

proc programNode*[T: Lua](a: var Ast[T]): AstNode[T] =
  ## create a head node for the program
  AstNode[T](kind: luaProgram)

proc emptyNode*[T: Lua](a: var Ast[T]): AstNode[T] =
  ## create an "empty" node suitable as a placeholder
  AstNode[T](kind: luaNil)

proc terminalNode*[T: Lua](a: var Ast[T]; term: Terminal): AstNode[T] =
  ## convert a terminal into an ast node
  case term.kind
  of Token:
    tokenNode(a, term.token, text = term.text)
  of None:
    AstNode[T](kind: luaNil)
  of Symbol:
    AstNode[T](kind: luaIdentifier, operand: a.strings.getOrIncl term.name)
  of String:
    AstNode[T](kind: luaString, operand: a.strings.getOrIncl term.strVal)
  of Float:
    AstNode[T](kind: luaNumber,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.floatVal))
  of Integer:
    AstNode[T](kind: luaNumber,  # transcode it to a float...
               operand: a.numbers.getOrIncl cast[BiggestInt](float term.intVal))
  of Boolean:
    case term.boolVal
    of true:
      AstNode[T](kind: luaTrue,
                 operand: a.numbers.getOrIncl cast[BiggestInt](term.boolVal))
    of false:
      AstNode[T](kind: luaFalse,
                 operand: a.numbers.getOrIncl cast[BiggestInt](term.boolVal))

proc composeCall*[T: Lua](fun: Function[T]): Ast[T] =
  ## create a call of the given function
  result.nodes.add:
    terminalNode(result, Terminal(kind: Token, token: luaFunctionCall))
  result =
    result.append(Terminal(kind: Symbol, name: fun.ident), parent = 0)

proc isFunctionSymbol*[T: Lua](a: Ast[T]; index: int): bool {.deprecated: "nonsensical".} =
  if index > 0 and index < a.high:
    if not a[index].isParent:
      result = a[index].isSymbol and a[index-1].kind == luaFunctionCall

when false:
  proc toAst[T: Lua](prod: Production[T]): Ast[T] =
    ## map a grammatical production to matching ast
    result.nodes = newSeqOfCap[AstNode[T]](prod.len)
    for component in prod.items:
      result.nodes.add:
        case component.kind
        of ckRule:
          emptyNode[T](result)
        of ckToken:
          AstNode[T](kind: component.token)
        of ckTerminal:
          terminalNode[T](result, component.term)

proc toAst[T: Lua](node: TsLuaNode; s: string): Ast[T] =
  case node.kind
  of luaIdentifier, luaPropertyIdentifier:
    result = result.append Terminal(kind: Symbol, name: s[node])
  of luaParents:
    #echo "parent kind ", node.kind, " text ", s[node]
    result = result.append Terminal(kind: Token,
                                       token: node.kind)

    # transcribe the children directly
    for i in 0..<node.len(unnamed = true):
      template item: TsLuaNode = node[i, unnamed = true]
      # create each child and add them to the parent
      if item.isNamed:
        result = result.append(toAst[T](item, s), parent = 0)
      else:
        let sym = Terminal(kind: Symbol, name: s[node{i}])
        result = result.append(sym, parent = 0)

  of luaTokenKinds:
    result = result.append Terminal(kind: Token,
                                       text: s[node],
                                       token: node.kind)
  of luaComment:
    discard
  of luaFalse:
    result = result.append: term false
  of luaTrue:
    result = result.append: term true
  of luaNumber:
    result = result.append: term parseFloat(s[node])
  else:
    raise Defect.newException "unimplemented node: " & $node.kind

proc newLuaProgram*(s: string): Program[Lua] =
  ## working around cps `()` operator shenanigans
  let tree = parseTsLuaString s
  if tree.kind != luaProgram:
    raise ValueError.newException "expected a program; got " & $tree.kind
  else:
    result = newProgram toAst[Lua](tree, s)

proc parseToken*[T: Lua](s: string): LuaNodeKind =
  case s
  of "if":            luaIfTok
  of "else":          luaElseTok
  of "elseif":        luaElseifTok
  of "then":          luaThentok
  of "end":           luaEndTok
  of "(":             luaLParTok
  of ")":             luaRParTok
  of ";":             luaSemicolonTok
  of "return":        luaReturnTok
  of "not":           luaNotTok
  of "and":           luaAndTok
  of "or":            luaOrTok
  else:
    raise ValueError.newException "unsupported token: `$#`" % [ s ]

proc parseLuaToken*(s: string): int16 =
  ## generic-free lua token parser for use in grammars
  parseToken[Lua](s).int16

proc initLocals*[V](values: openArray[(string, V)]): SymbolSet[Lua, V] {.deprecated: "workaround for cps".} =
  ## convert an openArray of (name, value) pairs into Locals
  ## suitable for evaluate()
  initSymbolSet[Lua, V](values)

when compileOption"threads":
  import gread/cluster
  import gread/generation

  proc noop(c: C): C {.cpsMagic.} = c

  proc worker*(args: Work[Lua, LuaValue]) {.cps: C.} =
    let lua = newLua args.core
    var evo: Evolver[Lua, LuaValue]
    initEvolver(evo, lua, args.tableau)
    if args.rng.isSome:
      evo.rng = get args.rng
    evo.name = args.name
    evo.operators = args.operators
    evo.dataset = args.dataset
    evo.core = lua.core
    evo.fitone = args.fitone
    evo.fitmany = args.fitmany
    evo.population = evo.randomPop()
    resetParsimony evo.population

    var leader: Hash
    var evoTime = getTime()
    while evo.population.generations.int <= evo.tableau.maxGenerations:
      noop() # give other evolvers a chance

      search(args, evo.population)   # fresh meat from other threads

      let stale = randomMember(evo.population, evo.rng)
      share(args, stale.program)

      for discovery in evo.generation():
        discard

      if evo.population.generations.int mod args.stats == 0:
        dumpStats(evo, evoTime)
        clearStats evo

    while evo.population.len > 0:
      share(args, randomRemoval(evo.population, evo.rng))

    quit 0

proc initLuaGrammar*(gram: var Grammar; syntax: string) =
  mixin initGrammar
  initGrammar(gram, parseLuaToken, syntax)
