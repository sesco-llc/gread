import std/times
import std/strformat
import std/sequtils
import std/math
import std/hashes
import std/options
import std/strutils

import pkg/lunacy
import pkg/adix/lptabz
import pkg/adix/stat
import pkg/balls

import gread/spec
import gread/ast
import gread/population
import gread/programs
import gread/maths except variance
import gread/primitives
import gread/data
import gread/evolver

export stat
export lptabz
export lunacy

const
  semanticErrorsAreFatal = false
  initialCacheSize = 32*1024

type
  Fennel* = ref object
    core*: Option[int]
    primitives: Primitives[Fennel]
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

proc newFennel*(c: Primitives[Fennel]; core = none int): Fennel =
  ## reset a Fennel instance and prepare it for running programs
  result = Fennel(vm: newState(), primitives: c, core: core)
  init(result.cache, initialSize = initialCacheSize)
  clearStats result
  result.runs = 0

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

when false:
  proc render*(c: Primitives[Fennel]; a: Ast[Fennel]): string =
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
    while i < a.len:
      closeParens()
      maybeAddSpace()
      if a[i].isParent:
        result.add "("
        s.add i+sizeOfSubtree(a, i)      # pop when you get past index+size
      else:
        result.add render(c, a[i])
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
  if p.zombie or p.hash in fnl.cache: return NaN
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
      let stack = evaluate(fnl.vm, render(fnl.primitives, p.ast), locals)
      fnl.runtime.push (getTime() - began).inMilliseconds.float
      fnl.errors.push 0.0
      # score a resultant value if one was produced
      if not stack.isNil:
        result = fit(locals, stack.value)
    except LuaError as e:
      when semanticErrorsAreFatal:
        debugEcho render(fnl.primitives, p.ast)
        debugEcho p
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
  let code = render(fnl.primitives, p.ast)
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
        checkpoint $value, "->", s
      if s.isValid:
        results.add s.float
    checkpoint "  stddev:", stddev(results)
    checkpoint "      ss: ", ss(results)
    fnl.dumpScore p

proc dumpPerformance*(fnl: Fennel; p: FProg; training: seq[(Locals, Score)];
                      fenfit: FenFit; samples = 8) =
  ## as in the prior overload, but consumes training data with associated
  ## ideal result values for each set of symbolic inputs; correlation is
  ## additionally provided
  if not p.isNil:
    var results: seq[float]
    var ideals: seq[float]
    for index, value in training.pairs:
      let s = evaluate(fnl, p, value[0], fenfit)
      if 0 == index mod samples or index == training.high:
        let delta = s.float
        if delta in [0.0, -0.0]:
          checkpoint fmt"{float(value[1]):>7.2f}"
        else:
          checkpoint fmt"{float(value[1]):>7.2f} -> {delta:>7.2f}     -> {sum(results):>7.2f}    {index}"
      if s.isValid:
        results.add s.float
        ideals.add abs(value[1].float)
    checkpoint "stddev:", stddev(results),
               "corr:", correlation(results, ideals),
               "ss:", ss(results, ideals),
               "of ideal:", sum(results) / sum(ideals)
    fnl.dumpScore p

proc dumpStats*(fnl: Fennel; pop: Population; evoTime: Time;
                gen: var FennelStat) =
  ## a threadsafe echo of some statistics regarding the vm and population
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
              total cache hits: {int fnl.hits.sum}
                cache hit rate: {fnl.hits.mean.percent}
                    cache size: {fnl.cache.len}
             foreign influence: {m.usurper}
               best generation: {m.bestGen}
             total generations: {m.generation}
             invention recency: {m.staleness.percent}
               generation time: {Score gen.mean} ms
                evolution time: {(getTime() - evoTime).inSeconds} sec
  """
  clearStats fnl
  clear gen

when compileOption"threads":
  import gread/cluster
  import gread/generation

  proc worker*(args: Work[Fennel, LuaValue]) {.thread, gcsafe.} =
    {.gcsafe.}:
      let fnl = newFennel(args.primitives, core = args.core)
      var evo: Evolver[Fennel, LuaValue]
      initEvolver(evo, fnl, args.tableau)
      evo.primitives = args.primitives
      evo.operators = args.operators
      evo.dataset = args.dataset
      evo.core = fnl.core
      evo.fitone = args.fitone
      evo.fitmany = args.fitmany
      evo.population = evo.randomPop()

      var leader: Hash
      var evoTime = getTime()
      var genTime: FennelStat
      while true:
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

        let clock = getTime()
        let invention = evo.generation()
        genTime.push (getTime() - clock).inMilliseconds.float

        if invention.isSome:
          let p = get invention
          if p.core.isNone and fnl.core.isSome:
            p.core = fnl.core

          if p.generation mod args.stats == 0:
            dumpStats(fnl, evo.population, evoTime, genTime)

          if p.score.isNaN:
            negativeCache(args, p)
