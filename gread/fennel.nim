import std/strformat
import std/sequtils
import std/math
import std/stats except variance
import std/hashes
import std/tables
import std/options
import std/strutils

import pkg/lunacy
import pkg/balls

import gread/spec
import gread/ast
import gread/population

export lunacy
export stats except variance

const
  semanticErrorsAreFatal = false

type
  Fennel* = ref object
    vm: PState
    runs*: uint
    cache: Table[Hash, Score]
    nans*: RunningStat
    errors*: RunningStat
    hits*: RunningStat

  Term* = Terminal[Fennel]
  Fun* = Function[Fennel]

  FProg* = Program[Fennel]
  FPop* = Population[Fennel]

  Locals* = object
    hash: Hash
    values: seq[(string, LuaValue)]

  FenFit = proc(locals: Locals; ideal: LuaValue): Score

proc initLocals*(values: openArray[(string, LuaValue)]): Locals =
  ## convert an openArray of (name, value) pairs into Locals
  ## suitable for evaluate()
  result.values = newSeqOfCap[(string, LuaValue)](values.len)
  for item in values.items:
    result.values.add item
  result.hash = hash result.values

proc clearStats*(fnl: Fennel) =
  clear fnl.nans
  clear fnl.errors
  clear fnl.hits

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
proc newFennel*(): Fennel =
  ## reset a Fennel instance and prepare it for running programs
  new result
  result.vm = newState()
  result.vm.openLibs()
  result.vm.checkLua result.vm.doString """fennel = require("fennel")""":
    let sham =
      """fennel.eval([==[$#]==], {compilerEnv=_G})""" % shims
    result.vm.checkLua result.vm.doString sham.cstring:
      discard
  clearStats result
  clear result.cache
  result.runs = 0

proc clearCache*(fnl: Fennel) =
  ## clear the execution cache of the Fennel instance
  clear fnl.cache
  clear fnl.hits

proc fun*(s: string; arity = 0; args = arity..int.high): Fun =
  Fun(ident: s, arity: max(arity, args.a), args: args)

proc term*(value: float): Term =
  Term(kind: Constant, ck: Float, floatVal: value)
proc term*(value: bool): Term =
  Term(kind: Constant, ck: Boolean, boolVal: value)
proc str*(value: string): Term =
  Term(kind: Constant, ck: String, strVal: value)
proc sym*(value: string): Term =
  Term(kind: Symbol, ident: value)
proc term*(value: int): Term =
  Term(kind: Constant, ck: ConstantKind.Integer, intVal: value)

proc pushGlobal*(vm: PState; name: string; value: Term) =
  ## push a name/value pair into the vm as a global
  case value.kind
  of Constant:
    case value.ck
    of Float:
      vm.pushNumber value.floatVal
    of Boolean:
      vm.pushNumber value.boolVal.float
    of String:
      vm.pushString value.strVal.cstring
    of ConstantKind.Integer:
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

proc `$`*(a: Ast[Fennel]): string

template joinWithSpaces[T](a: openArray[T]): string =
  map(a, `$`).join(" ")

proc `$`*(a: Ast[Fennel]): string =
  ## render fennel ast in a form that can be compiled
  case a.kind
  of Node:
    case a.node.ident
    of "{}":
      result = "{"
      result.add joinWithSpaces(a.args)
      result.add "}"
    of "[]":
      result = "["
      result.add joinWithSpaces(a.args)
      result.add "]"
    else:
      result = "("
      result.add a.node.ident
      result.add " "
      result.add joinWithSpaces(a.args)
      result.add ")"
  of Leaf:
    result = $a.leaf

proc `$`*(a: Program[Fennel]): string = $a.ast

proc `$`*(locals: Locals): string =
  result.add "["
  result.add mapIt(locals.values, $it[0] & "=" & $it[1]).join(" ")
  result.add "]"

proc evaluate(vm: PState; p: FProg; locals: Locals): LuaStack =
  ## compile and evaluate the program as fennel; the result of
  ## the expression is assigned to the variable `result`.
  auditLength p
  vm.pushGlobal("result", term 0.0)
  for name, value in locals.values.items:
    discard vm.push value
    vm.setGlobal name.cstring
  auditLength p
  let fennel = """
    result = fennel.eval([==[$#]==], {compilerEnv=_G})
  """ % [ $p ]
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
      let stack = evaluate(fnl.vm, p, locals)
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
        1.0
      else:
        # if it's valid, we'll cache it
        fnl.cache[h] = result
        0.0

proc dumpScore*(p: FProg) =
  if p.score.isValid:
    checkpoint fmt"{p.score} [{p.len}] at #{p.generation} from {p.source} for {p}"
  else:
    checkpoint fmt"... [{p.len}] at #{p.generation} from {p.source} for {p}"

proc dumpPerformance*(fnl: Fennel; p: FProg; training: seq[(Locals, Score)];
                      fenfit: FenFit) =
  if not p.isNil:
    var results: seq[float]
    for index, value in training.pairs:
      let s = evaluate(fnl, p, value[0], fenfit)
      if index < 8:
        checkpoint $value[0], "->", s
      if s.isValid:
        results.add s.float
    checkpoint stddev(results)
    dumpScore p
