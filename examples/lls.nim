import std/options
import std/os
import std/osproc
import std/strformat
import std/hashes
import std/random
import std/math

import gread
import gread/fennel except variance

import pkg/balls
import pkg/cps
import pkg/lunacy
import pkg/loony
import pkg/adix/lptabz

const
  goodEnough = 0.1      # termination condition
  statFrequency = 10000  # report after this many generations

var prims = newPrimitives[Fennel]()
prims.functions = @[
  fun("+", arity=2), fun("-", arity=2),
  fun("*", arity=2), fun("/", arity=2),
]
prims.constants = @[term 1.0, term 0.1]

# you can adjust these weights to change mutation rates
let operators = {
  randomCrossover[Fennel, LuaValue]:    1.0,
  pointPromotion[Fennel, LuaValue]:    15.0,
  removeOneLeaf[Fennel, LuaValue]:     30.0,
  appendOneLeaf[Fennel, LuaValue]:      2.0,
  pointMutation[Fennel, LuaValue]:      4.0,
  subtreeCrossover[Fennel, LuaValue]:  90.0,
}

const
  # given a line of (x, y) points, solve for y given x
  data = @[(1,6), (2,5), (3,7), (4,10)]

# preparing the data for use in the fitness()
prims.inputs.add [sym"x"]
var dataset: seq[Locals]
for (x, y) in data.items:
  dataset.add:
    initLocals [("x", x.toLuaValue), ("y", y.toLuaValue)]

proc fenfit(inputs: Locals; output: LuaValue): Score =
  ## fenfit gates program output such that producing a NaN will terminate
  ## scoring of the program early, and mark the program as invalid
  if output.kind == TNumber:
    output.toFloat
  else:
    NaN

proc fitone(fnl: Fennel; locals: Locals; p: FProg): Option[Score] =
  ## given a datapoint, run the program and return the residual
  let s = evaluate(fnl, p, locals, fenfit)
  if not s.isNaN:
    result =
      some:
        Score -abs(locals["y"].toFloat - s.float)

proc fitmany(fnl: Fennel; data: openArray[(Locals, Score)];
             p: FProg): Option[Score] =
  ## given several residuals, return the sum of squares
  var results = newSeqOfCap[float](data.len)
  for locals, s in data.items:
    if s.isNaN:
      return none Score
    else:
      results.add s
  if results.len > 0:
    result = some Score -ss(results)

when isMainModule:
  import std/sequtils
  import gread/cluster

  randomize()

  # the main loop monitors inventions
  proc main(work: Work; inputs, outputs: LoonyQueue[FProg]) =
    let fnl = newFennel(work.primitives)
    var best: Program[Fennel]
    while true:
      let p = pop inputs
      if p.isNil:
        sleep 10
      else:
        if FinestKnown in p.flags:
          if p.score.isValid:
            if best.isNil or best.score < p.score:
              best = p
              dumpPerformance(fnl, best, dataset, fenfit)
              if best.score > goodEnough:
                break
        push(outputs, p)

  # now setup the workers with their unique populations, etc.
  var tab = defaultTableau
  tab.useParsimony = false  # our scoring isn't -1.0..1.0
  tab.seedPopulation = 100
  tab.maxPopulation = 100
  tab.tournamentSize = 10
  tab.sharingRate = 6.0

  # each worker gets a Work object as input to its thread
  let clump = newCluster[Fennel, LuaValue]()
  var args = clump.initWork()
  initWork(args, tab, prims, operators = operators, dataset = dataset,
           fitone = fitone, fitmany = fitmany, stats = statFrequency)

  for core in 0..<countProcessors():
    clump.boot(whelp worker(args), args.core)
    clump.redress args
    break

  # run the main loop to gatekeep inventions
  let (inputs, outputs) = clump.programQueues()
  main(args, outputs, inputs)
