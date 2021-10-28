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
import pkg/lunacy
import pkg/loony
import pkg/adix/lptabz

const
  goodEnough = -0.0      # termination condition
  statFrequency = 10000  # report after this many generations

var prims = newPrimitives[Fennel]()
prims.functions = @[
  fun("+", arity=2), fun("-", arity=2),
  fun("*", arity=2), fun("/", arity=2),
]
prims.constants = @[term 1.0, term 0.1]

# you can adjust these weights to change mutation rates
let operators = {
  randomCrossover[Fennel, LuaValue]:  2.0,
  pointPromotion[Fennel, LuaValue]:   5.0,
  pointMutation[Fennel, LuaValue]:    4.0,
  subtreeCrossover[Fennel, LuaValue]: 90.0,
}

const
  # given a line of (x, y) points, solve for y given x
  data = @[(1,6), (2,5), (3,7), (4,10)]

# preparing the data for use in the fitness()
prims.inputs.add [sym"x"]
var training: seq[Locals]
var targets: LPTab[Hash, LuaValue]
init(targets, initialSize = data.len)
for (x, y) in data.items:
  var locals = initLocals [("x", x.toLuaValue)]
  targets[hash locals] = y.toLuaValue
  training.add locals

proc fenfit(inputs: Locals; output: LuaValue): Score =
  ## fenfit gates program output such that producing a NaN will terminate
  ## scoring of the program early, and mark the program as invalid
  if output.kind == TNumber:
    targets[hash inputs].toFloat - output.toFloat  # the residual
  else:
    NaN

proc fitone(fnl: Fennel; locals: Locals; p: FProg): Option[Score] =
  let s = evaluate(fnl, p, locals, fenfit)
  if not s.isNaN:
    result = some Score(-abs s)

proc fitmany(fnl: Fennel; ss: openArray[(Locals, Score)];
             p: FProg): Option[Score] =
  var results = newSeq[float](ss.len)
  for locals, s in ss.items:
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
  var tab = defaultTableau
  tab.useParsimony = false  # our scoring isn't -1.0..1.0
  tab.seedPopulation = 10000
  tab.maxPopulation = 10000

  # each worker gets a Work object as input to its thread
  let affinity = toSeq 0..<countProcessors()
  let clump = newCluster worker
  var args: Work[Fennel, LuaValue]
  initWork(args, tab, primitives = prims, operators = operators,
           dataset = training,
           fitone = fitone, fitmany = fitmany, stats = statFrequency)

  checkpoint fmt"seeding {affinity.len} threads..."
  clump.boot args
  clump.pin affinity

  # run the main loop to gatekeep inventions
  let (inputs, outputs) = clump.programQueues()
  main(args, outputs, inputs)
