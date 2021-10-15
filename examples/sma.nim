import std/os
import std/osproc
import std/strformat
import std/tables
import std/hashes
import std/random
import std/math

import gread/spec
import gread/population
import gread/tableau

import gread/fennel

import pkg/balls
import pkg/lunacy
import pkg/loony

const
  goodEnough = -0.0
  statFrequency = 2000
  poff = 8_000.0

var prims = newPrimitives[Fennel]()
prims.functions = @[
  fun("+", args=2..10),
  fun("/", args=2..10),
]
prims.constants = @[term 1.0]

const
  width = 8
  length = 5
var inputData: seq[array[width, float]]
for i in 0..<length:
  var value: array[width, float]
  for j in 0..<width:
    value[j] = float(rand 99)  # truncate
  inputData.add value

# convert the json into lua values
var training: seq[(Locals, Score)]
var targets: Table[Hash, LuaValue]
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

proc fenfit(inputs: Locals; output: LuaValue): Score =
  if output.kind == TNumber:
    # delta between output and the target for the given inputs
    output.toFloat - targets[hash inputs].toFloat
  else:
    NaN

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

  proc main(tab: Tableau; inputs, outputs: LoonyQueue[FProg]) =

    let fnl = newFennel()
    var pop = newPopulation(fnl, tab, prims)
    pop.fitness = fitness

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

  randomize()

  let
    tab =
      Tableau(seedPopulation:   1000, maxPopulation: 1000,
              maxGenerations: 500_000, seedProgramSize: 5,
              tournamentSize: 6, useParsimony: on)

  var args = initWork(tab, prims, stats = statFrequency, poff = poff)
  args.fitness = fitness
  args.io = (newLoonyQueue[FProg](), newLoonyQueue[FProg]())

  var threads: seq[Thread[Work]]
  newSeq(threads, countProcessors())

  checkpoint fmt"seeding {threads.len} threads..."
  for thread in threads.mitems:
    createThread(thread, worker, args)

  main(tab, args.io.outputs, args.io.inputs)

  for thread in threads.mitems:
    joinThread thread
