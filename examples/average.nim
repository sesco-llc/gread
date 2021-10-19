import std/strformat
import std/json
import std/tables
import std/hashes
import std/times
import std/random
import std/options
import std/math
import std/algorithm
import std/strutils

import gread/spec
import gread/population
import gread/fertilizer
import gread/tournament
import gread/generation
import gread/tableau
import gread/ast
import gread/primitives
import gread/maths
import gread/programs
import gread/operators

import gread/fennel

import pkg/balls
import pkg/lunacy
import pkg/lunacy/json as shhh

randomize()

var
  tab =
    Tableau(seedPopulation:   500, maxPopulation:   2000,
            maxGenerations: 500_000, seedProgramSize: 5,
            tournamentSize: 6, useParsimony: false)

const
  goodEnough = -0.5
  dataInaccurate = false
  sizePenalty = 0.02
  statFrequency = 5000
  manyArgs = 2
  poff = 8_000.0

var
  parsimonyCoefficient = -sizePenalty
  fnl: Fennel
  pop: FPop
  prims = newPrimitives[Fennel]()

let operatorWeights = {
  randomCrossover[Fennel]: 0.01,
  pointPromotion[Fennel]: 0.02,
  addOrRemoveLeaves[Fennel]: 0.04,
  pointMutation[Fennel]: 0.10,
  subtreeCrossover[Fennel]: 0.90,
}

prims.functions = @[
  fun("+", args=2..manyArgs), fun("-", args=2..manyArgs),
  fun("*", args=2..manyArgs), fun("/", args=2..manyArgs),
]
prims.constants = @[term 0.0, term 1.0, term 2.0]
prims.inputs.add @[sym"hi", sym"lo"]

var
  # we want to make a function that returns the median of `lo` and `hi` inputs
  inputData = @[
    # the training data is also the test data; no hold-outs, everybody fights
    (%* {"hi": 24.0,   "lo": 7.0},      15.5),
    (%* {"hi": 11.0,   "lo": 6.0},       8.5),
    (%* {"hi": 98.0,   "lo": 71.0},     84.5),

  ]

when dataInaccurate:
  inputData.add @[
    # not quite perfect data doesn't matter; we'll find the best approximation
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
var targets: Table[Hash, LuaValue]
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
    # apply some pressure on program size
    block:
      if tab.useParsimony and not parsimonyCoefficient.isNaN:
        s = max(-poff, s)
        s -= max(0.0, parsimonyCoefficient * p.len.float)
        break
      s -= sizePenalty * p.len.float
  result = Score: s

template dumpStats() {.dirty.} =
  var lengths = newSeqOfCap[int](pop.len)
  var scores = newSeqOfCap[float](pop.len)
  var validity = newSeqOfCap[float](pop.len)
  var ages = newSeqOfCap[int](pop.len)
  for p in pop.mitems:
    p.score = fitness(fnl, p)
    lengths.add p.len
    ages.add pop.generations - p.generation
    if p.score.isValid:
      scores.add: p.score
      validity.add 1.0
    else:
      validity.add 0.0
  let bestSize =
    if pop.fittest.isNil:
      "n/a"
    else:
      $pop.fittest.len
  checkpoint fmt"""
          virtual machine runs: {fnl.runs}
         total population size: {pop.len}
            average age in pop: {avg(ages)}
          validity rate in pop: {avg(validity).percent}
         program size variance: {variance(lengths)}
           average valid score: {Score avg(scores)}
             best score in pop: {Score pop.best}
          average program size: {avg(lengths)}
          size of best program: {bestSize}
         parsimony coefficient: {Score parsimonyCoefficient}
            insufficiency rate: {fnl.nans.mean.percent}
           semantic error rate: {fnl.errors.mean.percent}
              total cache hits: {int fnl.hits.sum}
                cache hit rate: {fnl.hits.mean.percent}
             total generations: {pop.generations}
                evolution time: {(getTime() - genTime).inSeconds} sec
  """
  clearStats fnl

var genTime = getTime()

proc dumpScore(p: FProg) =
  if p.score.isValid:
    checkpoint fmt"{p.score} [{p.len}] at #{p.generation} for {p}"
  else:
    checkpoint fmt"... [{p.len}] at #{p.generation} for {p}"

proc dumpPerformance(p: FProg) =
  if not p.isNil:
    for index, value in training.pairs:
      checkpoint inputData[index][0], "->", fitone(p, value[0])
    dumpScore p

suite "simulation":
  block:
    ## initialize fennel
    fnl = newFennel()

  block:
    ## created a random population of programs
    checkpoint "creating", tab.seedPopulation, "random programs..."
    pop = randomPop(fnl, tab, prims)
    pop.operators = operatorWeights
    pop.fitness = fitness

  block:
    ## dumped some statistics
    dumpStats()

  var best = Score NaN
  block:
    ## ran until we can average two numbers
    while pop.generations < tab.maxGenerations:
      if pop.best.isValid and pop.best >= goodEnough:
        break
      let p = generation pop
      if p.score > best:
        best = pop.best
        checkpoint pop.best, "at #" & $p.generation, "for", $p
        dumpPerformance p

      if pop.generations mod statFrequency == 0:
        dumpStats()
        dumpPerformance pop.fittest

      if tab.useParsimony:
        parsimonyCoefficient = pop.parsimony(poff)

  block:
    ## showed the top-10 programs
    for score, p in pop.top(10):
      dumpScore p

  block:
    ## performance of best program
    dumpPerformance pop.fittest

  block:
    ## dumped some statistics
    dumpStats()
