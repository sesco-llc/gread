import std/strformat
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
  goodEnough = -0.5          ## quit when you reach this score
  dataInaccurate = false     ## use faulty data
  statFrequency = 1000       ## how often to output statistics

# define the symbols in our evolved code
var prims = newPrimitives[Fennel]()
prims.functions = @[
  fun("+", args=2..2), fun("-", args=2..2),
  fun("*", args=2..2), fun("/", args=2..2),
]
prims.constants = @[term 0.0, term 1.0, term 2.0]
prims.inputs.add @[sym"hi", sym"lo"]

# no point in slowing down this simple example
var tab = defaultTableau
tab.useParsimony = false

# define the different ways in which we evolve, and their weights
let
  operators = {
    randomCrossover[Fennel]: 0.01,
    pointPromotion[Fennel]: 0.02,
    addOrRemoveLeaves[Fennel]: 0.04,
    pointMutation[Fennel]: 0.10,
    subtreeCrossover[Fennel]: 0.90,
  }

var
  fnl = newFennel()
  pop: FPop
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
var training: seq[(Locals, Score)]
var targets: LPTab[Hash, LuaValue]
init(targets, initialSize = inputData.len)
for (js, ideal) in inputData.items:
  var pairs: seq[(string, LuaValue)]
  for name, value in js.pairs:
    pairs.add (name, value.toLuaValue)
  var locals = initLocals pairs
  let luaIdeal = ideal.toLuaValue
  training.add (locals, Score ideal)
  targets[hash locals] = luaIdeal

proc fenfit(inputs: Locals; output: LuaValue): Score =
  if output.kind == TNumber:
    # delta between output and the target for the given inputs
    output.toFloat - targets[hash inputs].toFloat
  else:
    NaN

proc fitone(p: FProg; locals: Locals): Score =
  ## convenience capture
  evaluate(fnl, p, locals, fenfit)

proc fitness(fnl: Fennel; p: FProg): Score =
  var results = newSeq[float](training.len)
  var s = NaN
  for (locals, ideal) in training.items:
    s = fitone(p, locals)
    if s.isNaN:
      return Score NaN
    else:
      results.add s
  if results.len > 0:
    s = -(stddev results)
  result = Score: s

template dumpStats() {.dirty.} =
  dumpStats(fnl, pop, evo, genTime)

template dumpPerformance(p: FProg) {.dirty.} =
  dumpPerformance(fnl, p, training, fenfit)

suite "simulation":
  var evo = getTime()
  var genTime: FennelStat

  block:
    ## created a random population of programs
    checkpoint "creating", tab.seedPopulation, "random programs..."
    pop = randomPop(fnl, tab, prims)
    pop.operators = operators
    pop.fitness = fitness

  block:
    ## dumped some statistics
    dumpStats()

  evo = getTime()
  var best = Score NaN
  block:
    ## ran until we can average two numbers
    while pop.generations < tab.maxGenerations:
      if pop.best.isValid and pop.best >= goodEnough:
        break
      let clock = getTime()
      let p = generation pop
      genTime.push (getTime() - clock).inMilliseconds.float
      if p.score > best:
        best = pop.best
        dumpPerformance p

      if pop.generations mod statFrequency == 0:
        dumpStats()
        dumpPerformance pop.fittest

  block:
    ## showed the top-10 programs
    for score, p in pop.top(10):
      dumpScore p

  block:
    ## dumped some statistics
    dumpStats()

  block:
    ## performance of best program
    dumpPerformance pop.fittest
