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
    let score = lua.evaluate(p, locals)
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
    let score = lua.evaluate(p, locals)
    check score.float == 8.0

  block:
    ## parse lua grammar
    var gram: Grammar
    initGrammar[Lua](gram, luaGrammar)
    for name, production in gram.pairs:
      if name == "terminate":
        checkpoint production
    let geno = randomGenome(randState(), 2000)
    let (pc, ast) = πGE[Lua](gram, geno)
    checkpoint "program counter: ", pc
    var p = newProgram(ast, geno[0..<pc.int])
    let s = $p
    checkpoint s
    checkpoint "genome length: ", p.genome.len
    let (pc1, ast1) = πGE[Lua](gram, p.genome)
    check $p == $newProgram(ast1, p.genome[0..<pc1.int])
    var locals = initLocals:
      {
        "a": 3.toLuaValue,
        "b": 5.toLuaValue,
      }
    let score = lua.evaluate(p, locals)
    checkpoint $score
    check not score.float.isNaN
