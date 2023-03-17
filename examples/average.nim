import std/algorithm
import std/hashes
import std/json
import std/math
import std/options
import std/random
import std/sequtils
import std/strformat

import gread
import gread/fennel

import pkg/balls
import pkg/lunacy
import pkg/lunacy/json as shhh


const
  dataLength = 20
  greadSeed {.intdefine.} = 0
  goodEnough = -0.00         ## quit when you reach this score
  statFrequency = 10000      ## how often to output statistics

  # define the grammar for our programs; note the balancing
  averageGrammar = """
<start>            ::= <n-expr>

# numeric symbols
#<n-value>          ::= "hi" | "lo" | "0.5" | "2.0" | "0.0" | "1.0"
<n-value>          ::= "hi" | "lo"

# n-arity operators upon numeric expressions
<n-nop>            ::= "+" | "-" | "*" | "/"

# perform n-arity operation on two or more numeric expressions
<n-expr-narity>    ::= ( <n-nop> <n-expr> <n-args> )
<n-expr-narity>    ::= <n-value>

# numeric arguments are arity/n
<n-args>           ::= <n-expr> | <n-expr> <n-args>
# numeric arguments are arity/2
#<n-args>           ::= <n-expr>
<n-args>           ::= <n-value>

# balancing numeric expressions
<n-expr>           ::= <n-expr-narity>
#<n-expr>           ::= <n-value>
  """

var gram: Grammar
initFennelGrammar(gram, averageGrammar)

# no point in slowing down this simple example
var tab = defaultTableau
tab -= {UseParsimony}
tab -= {RequireValid}
tab.seedProgramSize = 400
tab.seedPopulation = 400
tab.maxPopulation = tab.seedPopulation
tab.tournamentSize = max(2, int(0.03 * tab.maxPopulation.float))
tab.maxGenerations = 100_000

# allow reproducability
if greadSeed == 0:
  checkpoint "rng was randomized"
  randomize()
else:
  checkpoint fmt"rng was seeded with {greadSeed}"
  randomize(greadSeed)

# define the different ways in which we evolve, and their weights
let operators = {
  geCrossover[Fennel, LuaValue]:        0.5,
  #geNoise1pt0[Fennel, LuaValue]:        0.5,
  geNoise4pt0[Fennel, LuaValue]:        0.5,
  #subtreeXover[Fennel, LuaValue]:       0.5,
  #randomSubtreeXover[Fennel, LuaValue]: 0.5,
  #randomCrossover[Fennel, LuaValue]:    0.5,
}

# we want to make a function that returns the median of `lo` and `hi` inputs
var fnl = newFennel()
var evo: Evolver[Fennel, LuaValue]

var inputData = newSeqOfCap[(JsonNode, float)](dataLength)

block:
  # the training data is also the test data; no hold-outs, everybody fights
  for n in 1..dataLength:
    var a, b: float
    while a == b:
      (a, b) = (rand(0.0 .. 1.0), rand(0.0 .. 1.0))
      (a, b) = (min(a, b), max(a, b))
    inputData.add (%* {"lo": a, "hi": b}, avg([a, b]))

# convert the json into lua values;
var training: seq[Locals]
for (js, ideal) in inputData.items:
  var paired: seq[(string, LuaValue)]
  for name, value in js.pairs:
    paired.add (name, value.toLuaValue)
  paired.add ("ideal", ideal.toLuaValue)
  training.add:
    initLocals paired

proc fitone(fnl: Fennel; locals: Locals; p: var FProg): Option[LuaValue] =
  ## convenience capture
  let s = evaluate(fnl, p, locals)
  if s.isValid:
    result =
      some:
        toLuaValue -abs(locals["ideal"].toFloat - s.float)

proc fitmany(fnl: Fennel; iter: iterator(): (ptr Locals, ptr LuaValue);
             p: FProg): Option[LuaValue] =
  var results: seq[float]
  for locals, s in iter():
    if s[].isValid:
      results.add s[]
    else:
      return none LuaValue
  if results.len > 0:
    let s = toLuaValue -ss(results)
    if s.isValid:
      result = some s

proc dumpPopulation(population: Population[Fennel]) =
  var programs = toSeq population
  sort programs
  for program in programs.mitems:
    dumpScore program

suite "simulation":
  block:
    ## created a random population of programs
    checkpoint "creating", tab.seedPopulation, "random programs..."
    initEvolver(evo, fnl, tab)
    evo.strength = strength
    evo.grammar = gram
    evo.operators = operators
    evo.dataset = training
    evo.fitone = fitone
    evo.fitmany = fitmany
    evo.population = evo.randomPop()

  block:
    ## dumped the initial population
    dumpPopulation evo.population

  block:
    ## dumped some statistics
    evo.dumpStats()

  var best = NaN
  block:
    ## ran until we can average two numbers
    var seen: GreadSet[Hash]
    initGreadSet seen
    while evo.generation < tab.maxGenerations:
      for discovery in evo.generation():
        discard
      if evo.fittest.isSome:
        let p = get evo.fittest
        if not seen.containsOrIncl(p.hash):
          let s = p.score
          if s > best or best.isNaN:
            best = s
            if best > goodEnough or best.almostEqual(goodEnough):
              break
            checkpoint "new leader!"
            dumpPopulation evo.population

      if evo.generation mod statFrequency == 0:
        evo.dumpStats()

  block:
    ## dumped the final population
    dumpPopulation evo.population

  block:
    ## dumped some statistics
    evo.dumpStats()
