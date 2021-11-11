import std/json
import std/options
import std/strutils
import std/sequtils
import std/math

import pkg/balls

import gread

import gread/fennel

var c = newPrimitives[Fennel]()
c.functions = @[
  fun("+", arity=2), fun("-", arity=2),
  fun("*", arity=2), fun("/", arity=2),
]

proc fenfit(inputs: Locals; output: LuaValue): Score =
  Score:
    if output.kind == TNumber:
      output.toFloat
    else:
      NaN

when false:
  proc fitone(fnl: Fennel; locals: Locals; p: FProg): Option[Score] =
    let s = evaluate(fnl, p, locals, fenfit)
    if not s.isNaN:
      var fib = locals["a"].toFloat + locals["b"].toFloat
      result =
        some:
          Score -abs(fib - s.float)

  proc fitmany(fnl: Fennel; data: openArray[(Locals, Score)];
               p: FProg): Option[Score] =
    var esses = newSeqOfCap[float](data.len)
    var ideal = newSeqOfCap[float](data.len)
    var total: float
    for locals, s in data.items:
      if s.isNaN:
        return none Score
      else:
        ideal.add locals["a"].toFloat + locals["b"].toFloat
        esses.add s
    if esses.len > 0:
      result =
        some:
          Score -ss(esses)

suite "basic fennel stuff":
  var fnl: Fennel
  block:
    ## setup a lua vm
    fnl = newFennel c

  block:
    ## tree-sitter suite
    when greadTS:
      suite "tree-sitter":
        var p: FProg
        block:
          ## parse a fennel program with multi-symbols
          const program = "(/ math.pi math.pi)"
          p = newProgram(c, program)
          check $p == "(/ math.pi math.pi)"

        block:
          ## parse a fennel program
          const program = "(+ 1   2.0  )"
          p = newProgram(c, program)
          check $p == "(+ 1.0 2.0)"

        block:
          ## run a fennel program
          var locals: Locals
          let score = fnl.evaluate(p, locals, fenfit)
          check score.float == 3.0

        block:
          ## serde some fennel ast
          let popsicle = freeze p
          var puddle: FProg
          thaw(popsicle, puddle)
          check c.render(p.ast) == c.render(puddle.ast)

        block:
          ## run a fennel program with inputs
          const program = "(+ a b)"
          p = newProgram(c, program)
          check $p == "(+ a b)"
          # [("a", 3.toLuaValue), ("b", 5.toLuaValue)]
          var locals = initLocals:
            {
              "a": 3.toLuaValue,
              "b": 5.toLuaValue,
            }
          let score = fnl.evaluate(p, locals, fenfit)
          check score.float == 8.0
