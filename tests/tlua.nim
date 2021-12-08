import std/math
import std/random
import std/json
import std/options

import pkg/balls
import pkg/frosty/streams as brrr

import gread
import gread/lua

randomize()

const
  luaGrammar = """
    <start>        ::= <expr>
    <expr>         ::= if <boolexpr> then <terminate> else <terminate> end
    <boolexpr>     ::= ( not <boolexpr> )
    <boolexpr>     ::= ( <boolexpr> or <boolexpr> )
    <boolexpr>     ::= ( <numexpr> <boolop> <numexpr> )
    <boolexpr>     ::= ( <boolexpr> and <boolexpr> )
    <numexpr>      ::= ( <numexpr> <numbop> <numexpr> )
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <boolop>       ::= ">" | "<" | "~=" | "==" | "<=" | ">="
    <numbop>       ::= "+" | "-" | "*" | "/"
    <value>        ::= "1" | "0" | "0.5" | "2"
    <value>        ::= "a" | "b"
    <terminate>    ::= return <numexpr>
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
    lua = newLua()

  block:
    ## parse a lua program with field expressions
    const program = "math.pi / math.pi"
    p = newLuaProgram program
    checkpoint $p
    check $p == "(math.pi / math.pi)"

  block:
    ## parse a lua program
    const program = "return 1 + 2.0"
    p = newLuaProgram program
    checkpoint $p
    check $p == "return 1.0 + 2.0"

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
    checkpoint render(p.ast)
    check render(p.ast) == render(puddle.ast)

  block:
    ## run a lua program with inputs
    const program = "return a + b"
    p = newLuaProgram program
    checkpoint $p
    check $p == "return a + b"
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
    let geno = randomGenome(2000)
    let (pc, ast) = gram.πGE(geno)
    checkpoint "program counter: ", pc
    var p = newProgram(ast, geno[0..<pc.int])
    let s = $p
    checkpoint s
    checkpoint "genome length: ", p.genome.len
    let (pc1, ast1) = gram.πGE(p.genome)
    check $p == $newProgram(ast1, p.genome[0..<pc1.int])
    var locals = initLocals:
      {
        "a": 3.toLuaValue,
        "b": 5.toLuaValue,
      }
    let score = lua.evaluate(p, locals, luafit)
    checkpoint $score
    check not score.float.isNaN
