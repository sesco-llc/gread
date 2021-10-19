import std/os
import std/osproc
import std/strformat
import std/tables
import std/hashes
import std/random
import std/math

import gread
import gread/fennel

import pkg/balls
import pkg/lunacy
import pkg/loony

const
  goodEnough = -0.0
  statFrequency = 2000

var prims = newPrimitives[Fennel]()
prims.functions = @[
  fun("+", args=2..2), fun("-", args=2..2),
  fun("*", args=2..2), fun("/", args=2..2),
  fun("%", args=2..2), fun("^", args=2..2),
]
prims.constants = @[term 1.0, term 0.1]

let operators = {
  randomCrossover[Fennel]: 0.02,
  pointPromotion[Fennel]: 0.05,
  addOrRemoveLeaves[Fennel]: 0.07,
  pointMutation[Fennel]: 0.04,
  subtreeCrossover[Fennel]: 0.90,
}

const
  data = @[(1,6), (2,5), (3,7), (4,10)]

prims.inputs.add [sym"x"]
var training: seq[Locals]
var targets: Table[Hash, LuaValue]
for (x, y) in data.items:
  var locals = initLocals [("x", x.toLuaValue)]
  targets[hash locals] = y.toLuaValue
  training.add locals

proc fenfit(inputs: Locals; output: LuaValue): Score =
  if output.kind == TNumber:
    targets[hash inputs].toFloat - output.toFloat  # the residual
  else:
    NaN

proc fitness(fnl: Fennel; p: FProg): Score =
  var results = newSeq[float](training.len)
  var s = NaN
  for locals in training.items:
    s = evaluate(fnl, p, locals, fenfit)
    if s.isNaN:
      return Score NaN
    else:
      results.add s
  if results.len > 0:
    s = -ss(results)
  result = Score: s

when isMainModule:

  randomize()

  # the main loop monitors inventions
  proc main(tab: Tableau; inputs, outputs: LoonyQueue[FProg]) =
    let fnl = newFennel()
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
  let tab = defaultTableau
  var args = initWork(tab, prims, operators, fitness, stats = statFrequency)

  var threads: seq[Thread[Work]]
  newSeq(threads, countProcessors())

  checkpoint fmt"seeding {threads.len} threads..."
  for thread in threads.mitems:
    createThread(thread, worker, args)

  main(tab, args.io.outputs, args.io.inputs)

  for thread in threads.mitems:
    joinThread thread
