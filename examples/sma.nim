import std/os
import std/osproc
import std/strformat
import std/hashes
import std/random
import std/math

import gread
import gread/fennel

import pkg/balls
import pkg/lunacy
import pkg/loony
import pkg/adix/lptabz

const
  goodEnough = -0.0
  statFrequency = 2000

var prims = newPrimitives[Fennel]()
prims.functions = @[
  fun("+", args=2..10),
  fun("/", args=2..10),
]
prims.constants = @[term 1.0]

let operators = {
  randomCrossover[Fennel]: 0.01,
  pointPromotion[Fennel]: 0.02,
  pointMutation[Fennel]: 0.10,
  subtreeCrossover[Fennel]: 0.90,
}

const
  width = 8
  length = 5
var inputData: seq[array[width, float]]
for i in 0..<length:
  var value: array[width, float]
  for j in 0..<width:
    value[j] = float(rand 99)  # truncate
  inputData.add value

# turn the input data into forms we can consume in fitness()
var training: seq[(Locals, Score)]
var targets: LPTab[Hash, LuaValue]
init(targets, initialSize = inputData.len)
for arr in inputData.items:
  var pairs: seq[(string, LuaValue)]
  let ideal = avg(arr[^5..^1]).float
  for index, value in arr.pairs:
    let name = "v" & $index
    pairs.add (name, value.toLuaValue)
    prims.inputs.add sym(name)
  var locals = initLocals pairs
  let luaIdeal = ideal.toLuaValue
  training.add (locals, Score ideal)
  targets[hash locals] = luaIdeal

# fenfit lets us short-circuit when an individual datapoint fails
proc fenfit(inputs: Locals; output: LuaValue): Score =
  if output.kind == TNumber:
    # delta between output and the target for the given inputs
    output.toFloat - targets[hash inputs].toFloat
  else:
    NaN

# compute the fitness as stddev across all datapoints
proc fitness(fnl: Fennel; p: FProg): Score =
  var results = newSeq[float](training.len)
  var s = NaN
  for (locals, ideal) in training.items:
    s = evaluate(fnl, p, locals, fenfit)
    if s.isNaN:
      return Score NaN
    else:
      results.add s
  if results.len > 0:
    s = -(stddev results)
  result = Score: s

when isMainModule:

  randomize()

  # the main loop monitors inventions
  proc main(work: Work; inputs, outputs: LoonyQueue[FProg]) =
    let fnl = newFennel(work.primitives)
    var best = Score NaN
    while not best.isValid or best < goodEnough:
      let p = pop inputs
      if p.isNil:
        sleep 10
      else:
        if p.score > best or best.isNaN:
          best = p.score
          dumpPerformance(fnl, p, training, fenfit)
        push(outputs, p)

  # now setup the workers with their unique populations, etc.
  let
    tab =
      Tableau(seedPopulation:   1000, maxPopulation: 1000,
              maxGenerations: 500_000, seedProgramSize: 5,
              tournamentSize: 6, useParsimony: on)

  var args = initWork(tab, prims, operators, fitness, stats = statFrequency)
  var threads: seq[Thread[Work]]
  newSeq(threads, countProcessors())

  checkpoint fmt"seeding {threads.len} threads..."
  for thread in threads.mitems:
    createThread(thread, worker, args)

  main(args, args.io.outputs, args.io.inputs)

  for thread in threads.mitems:
    joinThread thread
