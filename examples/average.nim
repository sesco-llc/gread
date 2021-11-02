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
  goodEnough = -0.01         ## quit when you reach this score
  dataInaccurate = false     ## use faulty data
  statFrequency = 2000       ## how often to output statistics

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

# define the different ways in which we evolve, and their weights
let
  operators = {
    randomCrossover[Fennel, LuaValue]:   1.0,
    pointPromotion[Fennel, LuaValue]:    2.0,
    removeOneLeaf[Fennel, LuaValue]:     5.0,
    pointMutation[Fennel, LuaValue]:     5.0,
    subtreeCrossover[Fennel, LuaValue]: 90.0,
  }

var
  fnl = newFennel(prims)
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
var training: seq[(Locals, Score)]
var targets: LPTab[Hash, LuaValue]
init(targets, initialSize = inputData.len)
for (js, ideal) in inputData.items:
  var paired: seq[(string, LuaValue)]
  for name, value in js.pairs:
    paired.add (name, value.toLuaValue)
  var locals = initLocals paired
  let luaIdeal = ideal.toLuaValue
  training.add (locals, Score ideal)
  targets[hash locals] = luaIdeal

proc fenfit(inputs: Locals; output: LuaValue): Score =
  if output.kind == TNumber:
    # delta between output and the target for the given inputs
    output.toFloat - targets[hash inputs].toFloat
  else:
    NaN

proc fitone(fnl: Fennel; locals: Locals; p: FProg): Option[Score] =
  ## convenience capture
  let s = evaluate(fnl, p, locals, fenfit)
  if not s.isNaN and s notin [-Inf, Inf]:
    result = some Score(-abs s)

proc fitmany(fnl: Fennel; ss: openArray[(Locals, Score)];
             p: FProg): Option[Score] =
  var results = newSeqOfCap[float](ss.len)
  for locals, s in ss.items:
    if s.isNaN or s in [-Inf, Inf]:
      return none Score
    else:
      results.add s
  if results.len > 0:
    result = some Score -(stddev results)

template dumpStats() {.dirty.} =
  dumpStats(fnl, pop, et, genTime)

template dumpPerformance(p: FProg) {.dirty.} =
  dumpPerformance(fnl, p, training, fenfit)

suite "simulation":
  var et = getTime()
  var genTime: FennelStat

  block:
    ## created a random population of programs
    checkpoint "creating", tab.seedPopulation, "random programs..."
    initEvolver(evo, fnl, tab)
    evo.primitives = prims
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
  var best = Score NaN
  block:
    ## ran until we can average two numbers
    while pop.generations < tab.maxGenerations:
      if best.isValid and best >= goodEnough:
        break
      let clock = getTime()
      let invented = evo.generation()
      if invented.isSome:
        let p = get invented
        genTime.push (getTime() - clock).inMilliseconds.float
        if p.score > best or best.isNaN:
          best = p.score
          dumpPerformance p

      if pop.generations mod statFrequency == 0:
        dumpStats()

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
