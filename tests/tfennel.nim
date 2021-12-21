import std/math
import std/random
import std/json
import std/options

import pkg/balls
import pkg/frosty/streams as brrr

import gread
import gread/fennel

randomize()

const
  fennelGrammar = """
    <start>        ::= <expr>
    <expr>         ::= ( "if" <boolexpr> <expr> <expr> )
    <expr>         ::= <numexpr>
    <boolexpr>     ::= ( "not" <boolexpr> )
    <boolexpr>     ::= ( "or" <boolexpr> <boolexpr> )
    <boolexpr>     ::= ( "and" <boolexpr> <boolexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <boolexpr>     ::= ( <boolop> <numexpr> <numexpr> )
    <numexpr>      ::= ( <numbop> <numexpr> <numexpr> )
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
    <boolop>       ::= ">" | "<" | "~=" | "not=" | "=" | "<=" | ">="
    <numbop>       ::= "+" | "-" | "*" | "/"
    <value>        ::= "1" | "0" | "0.5" | "2"
    <value>        ::= "a" | "b"
  """

suite "basic fennel stuff":
  var fnl: Fennel
  var p: FProg
  block:
    ## setup a lua vm
    fnl = newFennel()

  block:
    ## parse a fennel program with multi-symbols
    const program = "(/ math.pi math.pi)"
    p = newFennelProgram program
    checkpoint $p
    check $p == "(/ math.pi math.pi)"

  block:
    ## parse a fennel program
    const program = "(+ 1   2.0  )"
    p = newFennelProgram program
    checkpoint $p
    check $p == "(+ 1.0 2.0)"

  block:
    ## run a fennel program
    var locals: Locals
    let score = fnl.evaluate(p, locals)
    checkpoint score
    check score.float == 3.0

  block:
    ## serde some fennel ast
    let popsicle = freeze p
    var puddle: FProg
    thaw(popsicle, puddle)
    checkpoint render(p.ast)
    check render(p.ast) == render(puddle.ast)

  block:
    ## run a fennel program with inputs
    const program = "(+ a b)"
    p = newFennelProgram program
    checkpoint $p
    check $p == "(+ a b)"
    # [("a", 3.toLuaValue), ("b", 5.toLuaValue)]
    var locals = initLocals:
      {
        "a": 3.toLuaValue,
        "b": 5.toLuaValue,
      }
    let score = fnl.evaluate(p, locals)
    check score.float == 8.0

  block:
    ## parse fennel grammar
    var gram: Grammar[Fennel]
    gram.initGrammar fennelGrammar
    for name, production in gram.pairs:
      if name == "start":
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
    let score = fnl.evaluate(p, locals)
    checkpoint $score
    check not score.float.isNaN
