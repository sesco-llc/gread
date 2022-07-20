import std/packedsets
import std/json
import std/hashes
import std/times
import std/random
import std/options
import std/math
import std/algorithm
import std/strutils

import gread
import gread/fennel

import pkg/balls
import pkg/lunacy
import pkg/lunacy/json as shhh
import pkg/adix/lptabz

randomize()

const
  goodEnough = -0.00         ## quit when you reach this score
  dataInaccurate = false     ## use faulty data
  statFrequency = 2000       ## how often to output statistics

  # define the grammar for our programs; note the balancing
  averageGrammar = """
    <start>        ::= <numexpr>
    <numexpr>      ::= ( <numbop> <numexpr> <numexpr> )
    <numexpr>      ::= <value>
    <numbop>       ::= "+" | "-" | "*" | "/"
    <value>        ::= "1" | "0" | "0.5" | "2"
    <value>        ::= "hi" | "lo"
  """

var gram: Grammar
initFennelGrammar(gram, averageGrammar)

# no point in slowing down this simple example
var tab = defaultTableau
tab.useParsimony = true
tab.seedProgramSize = 200
tab.seedPopulation = 500
tab.maxPopulation = 500
tab.tournamentSize = 10
tab.maxGenerations = 30_000
tab.requireValid = true

# define the different ways in which we evolve, and their weights
let operators = {
  geCrossover[Fennel, LuaValue]:   100.0,
  geMutation[Fennel, LuaValue]:     70.0,
  randomCrossover[Fennel, LuaValue]: 0.1,
}

var
  fnl = newFennel()
  pop: FPop
  evo: FEvo
  # we want to make a function that returns the median of `lo` and `hi` inputs
  inputData = @[
    # the training data is also the test data; no hold-outs, everybody fights
    (%* {"hi": 24.0,   "lo": 7.0},      15.5),
    (%* {"hi": 11.0,   "lo": 6.0},       8.5),
    (%* {"hi": 98.0,   "lo": 71.0},     84.5),
  ]

when dataInaccurate:
  # not quite perfect data doesn't matter; we'll find the best approximation
  inputData.add @[
    (%* {"hi": 298,    "lo": 171},     244.5),
    (%* {"hi": 65,     "lo": 60},       63.0),
    (%* {"hi": 9000,   "lo": 7000},   8017.0),
    (%* {"hi": 25.0,   "lo": 24.0},     24.8),
    (%* {"hi": 101.0,  "lo": 1.0},      49.0),
    (%* {"hi": 2200.0, "lo": 1000.0}, 1500.0),
  ]
else:
  inputData.add @[
    (%* {"hi": 298,    "lo": 171},     234.5),
    (%* {"hi": 65,     "lo": 60},       62.5),
    (%* {"hi": 7000,   "lo": 9000},   8000.0),
    (%* {"hi": 25.0,   "lo": 24.0},     24.5),
    (%* {"hi": 101.0,  "lo": 1.0},      51.0),
    (%* {"hi": 2200.0, "lo": 1000.0}, 1600.0),
  ]

# convert the json into lua values;
var training: seq[(Locals, LuaValue)]
for (js, ideal) in inputData.items:
  var paired: seq[(string, LuaValue)]
  for name, value in js.pairs:
    paired.add (name, value.toLuaValue)
  paired.add ("ideal", ideal.toLuaValue)
  var locals = initLocals paired
  training.add (locals, ideal.toLuaValue)

proc fitone(fnl: Fennel; locals: Locals; p: FProg): Option[LuaValue] =
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

template dumpStats() {.dirty.} =
  dumpStats(evo, et)

template dumpPerformance(p: FProg) {.dirty.} =
  dumpPerformance(fnl, p, training, samples = 1)

suite "simulation":
  var et = getTime()
  block:
    ## created a random population of programs
    checkpoint "creating", tab.seedPopulation, "random programs..."
    initEvolver(evo, fnl, tab)
    evo.grammar = gram
    evo.operators = operators
    evo.dataset = training
    evo.fitone = fitone
    evo.fitmany = fitmany
    evo.population = evo.randomPop()

    # we use `pop` in some closures, so we'll assign it here
    pop = evo.population

  block:
    ## dumped some statistics
    dumpStats()

  et = getTime()
  var best = NaN
  var lastGeneration: Generation
  block:
    ## ran until we can average two numbers
    var seen: PackedSet[Hash]
    while pop.generations < tab.maxGenerations:
      if best >= goodEnough:
        break
      for discovery in evo.generation():
        discard
      let p = pop.fittest
      if not p.isNil:
        if evo.cacheSize(p) == training.len:
          if not seen.containsOrIncl(p.hash):
            let s = p.score
            if s > best or best.isNaN:
              best = s
              dumpPerformance p

      if pop.generations mod statFrequency == 0:
        dumpStats()
    lastGeneration = pop.generations

  block:
    ## showed the top-10 programs
    for score, p in pop.top(10):
      fnl.dumpScore p

  block:
    ## dumped some statistics
    dumpStats()

  block:
    ## performance of best program
    dumpPerformance pop.fittest
    checkpoint "last generation: ", lastGeneration
