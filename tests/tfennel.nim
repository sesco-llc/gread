import std/times
import std/math
import std/random
import std/json
import std/options

import pkg/balls
import pkg/frosty/streams as brrr

import gread
import gread/decompile
import gread/fennel

const
  #expectedMapping = "(if (> 2.0 b ) 2.0 (- 0.0 a ) )"
  expectedMapping = "(if (or (not= 0.0 2.0 ) (not= 1.0 1.0 ) ) 0.0 0.5 )"
  badSeed = 47059
  fennelGrammar = """
    <start>        ::= <expr>
    <expr>         ::= ( "if" <boolexpr> <expr> <expr> )
    <expr>         ::= <numexpr>
    <expr>         ::= <numexpr>
    <boolexpr>     ::= ( "or" <boolexpr> <boolexpr> )
    <boolexpr>     ::= ( "and" <boolexpr> <boolexpr> )
    <boolexpr>     ::= ( <boolop> <expr> <expr> )
    <boolexpr>     ::= ( <boolop> <expr> <expr> )
    <boolexpr>     ::= ( <boolop> <expr> <expr> )
    <numexpr>      ::= ( <numbop> <expr> <expr> )
    <numexpr>      ::= <value>
    <numexpr>      ::= <value>
    <boolop>       ::= ">" | "<" | "~=" | "not=" | "=" | "<=" | ">="
    <numbop>       ::= "+" | "-" | "*" | "/"
    <value>        ::= "1.0" | "0.0" | "0.5" | "2.0"
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
    var gram: Grammar
    var rng = initRand(5)
    initFennelGrammar(gram, fennelGrammar)
    for name, production in gram.pairs:
      if name == "start":
        checkpoint production
    let geno = randomGenome(rng, 200)
    let (pc, ast) = ??GE[Fennel](gram, geno)
    checkpoint "program counter: ", pc
    var p = newProgram(ast, geno[0..<pc.int])
    let s = $p
    checkpoint s
    checkpoint "genome length: ", p.genome.len
    let (pc1, ast1) = ??GE[Fennel](gram, p.genome)
    check $p == $newProgram(ast1, p.genome[0..<pc1.int])
    var locals = initLocals:
      {
        "a": 3.toLuaValue,
        "b": 5.toLuaValue,
      }
    let score = fnl.evaluate(p, locals)
    checkpoint $score
    check not score.float.isNaN

  #[
    block:
      ## find some interesting programs
      var gram: Grammar
      initFennelGrammar(gram, fennelGrammar)
      for n in 1..50_000:
        var rng = initRand(n)
        let geno = randomGenome(rng, 100)
        try:
          let (pc, ast) = ??GE[Fennel](gram, geno)
          p = newProgram(ast, geno[0..<pc.int])
          if len($p) > 50:
            echo n, " -> ", $p
            break
        except ShortGenome:
          discard
      quit 0
  ]#

  block:
    ## mapping seems to be stable
    var s: string
    var p: Program[Fennel]
    var gram: Grammar
    block found:
      for n in badSeed..badSeed:
        var rng = initRand(n)
        let geno = randomGenome(rng, 100)
        try:
          for i in 1..10:
            randomize()
            initFennelGrammar(gram, fennelGrammar)
            let (pc, ast) = ??GE[Fennel](gram, geno)
            p = newProgram(ast, geno[0..<pc.int])
            s = $p
            check s == expectedMapping, "s was `" & s & "`"

            checkpoint s
            if s.len > 10 and p.genome.len > 60:
              checkpoint "genome length: ", p.genome.len
              checkpoint "n value: ", n
              break found
        except ShortGenome:
          discard

  #[
  block:
    ## decompilation is a thing
    var fnl = newFennel()
    var p: Program[Fennel]

    var gram: Grammar
    initFennelGrammar(gram, fennelGrammar)

    var rng = initRand(badSeed)
    let geno = randomGenome(rng, 100)

    let (pc, ast) = ??GE[Fennel](gram, geno)
    p = newProgram(ast, geno[0..<pc.int])
    let target = $p
    check target == expectedMapping

    rng = initRand(2)

    var et = getTime()
    var tab = defaultTableau
    tab.maxPopulation = 10
    tab.tournamentSize = 2
    tab.seedPopulation = tab.maxPopulation
    var evo = decompiler(fnl, tab, gram, expectedMapping, rng = rng)
    while true:
      for discovery in evo.generation():
        let generation = evo.population.generations.int
        if evo.fittest.isNone:
          fail"simply unfit"
        else:
          if $evo.fittest.get == target:
            checkpoint "decompiled program after ", generation, " generations"
            break
          elif 0 == generation mod 10_000:
            dumpStats(evo, et)
            echo $evo.fittest.get
            echo target
          elif generation >= 1_000_000:
            fail "unable to decompile program"

    dumpStats(evo, et)

    block:
      let geno = evo.fittest.get.genome
      let (pc, ast) = ??GE[Fennel](gram, geno)
      p = newProgram(ast, geno[0..<pc.int])
      let s = $p
      check s == target
    ]#
