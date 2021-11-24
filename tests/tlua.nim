import std/json
import std/options

import pkg/balls
import pkg/frosty/streams as brrr

import gread
import gread/lua

const
  luaGrammar = """
    <start>        ::= <terminate>
    <expr>         ::= if <boolexpr> then <expr> else <expr> end
    <boolexpr>     ::= ( <expr> <boolop> <expr> )
    <expr>         ::= <value>
    <boolop>       ::= ">" | "<" | "==" | "<=" | ">="
    <numbop>       ::= "+" | "-" | "*" | "/"
    <value>        ::= "1" | "0" | "0.5"
    <terminate>    ::= return <expr>
  """

var c = newPrimitives[Lua]()
c.functions = @[
  fun("+", arity=2), fun("-", arity=2),
  fun("*", arity=2), fun("/", arity=2),
]

proc luafit(inputs: Locals; output: LuaValue): Score =
  Score:
    if output.kind == TNumber:
      output.toFloat
    else:
      NaN

when false:
  proc fitone(lua: Lua; locals: Locals; p: LProg): Option[Score] =
    let s = evaluate(lua, p, locals, luafit)
    if not s.isNaN:
      var fib = locals["a"].toFloat + locals["b"].toFloat
      result =
        some:
          Score -abs(fib - s.float)

  proc fitmany(lua: Lua; data: openArray[(Locals, Score)];
               p: LProg): Option[Score] =
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

suite "basic lua stuff":
  var p: LProg
  var lua: Lua
  block:
    ## setup a lua vm
    lua = newLua c

  block:
    ## parse a lua program with field expressions
    const program = "math.pi / math.pi"
    p = newProgram(c, program)
    checkpoint $p
    check $p == "math.pi / math.pi"

  block:
    ## parse a lua program
    const program = "1 + 2.0"
    p = newProgram(c, program)
    checkpoint $p
    check $p == "1.0 + 2.0"

  block:
    ## run a lua program
    var locals: Locals
    let score = lua.evaluate(p, locals, luafit)
    checkpoint score
    check score.float == 3.0

  block:
    ## serde some lua ast
    let popsicle = freeze p
    var puddle: LProg
    thaw(popsicle, puddle)
    checkpoint c.render(p.ast)
    check c.render(p.ast) == c.render(puddle.ast)

  block:
    ## run a lua program with inputs
    const program = "a + b"
    p = newProgram(c, program)
    checkpoint $p
    check $p == "a + b"
    # [("a", 3.toLuaValue), ("b", 5.toLuaValue)]
    var locals = initLocals:
      {
        "a": 3.toLuaValue,
        "b": 5.toLuaValue,
      }
    let score = lua.evaluate(p, locals, luafit)
    check score.float == 8.0

  block:
    ## parse lua grammar
    var gram: Grammar[Lua]
    gram.initGrammar(luaGrammar)
    for name, production in gram.pairs:
      if name == "terminate":
        checkpoint production
