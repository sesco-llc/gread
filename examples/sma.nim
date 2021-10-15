import std/os
import std/osproc
import std/sequtils
import std/strformat
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
import gread/crossover
import gread/generation
import gread/tableau
import gread/ast

import gread/fennel

import pkg/balls
import pkg/lunacy
import pkg/loony

const
  goodEnough = -0.0
  statFrequency = 2000
  manyArgs = 2
  poff = 8_000.0

var
  prims = newPrimitives[Fennel]()

prims.functions = @[
  fun("+", args=2..manyArgs),
  fun("/", args=2..manyArgs),
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

template dumpStats(pop: var Population) {.dirty.} =
  var lengths = newSeqOfCap[int](pop.len)
  var scores = newSeqOfCap[float](pop.len)
  var validity = newSeqOfCap[float](pop.len)
  var ages = newSeqOfCap[int](pop.len)
  for p in pop.mitems:
    let s = pop.score(p)
    lengths.add p.len
    ages.add pop.generations - p.generation
    if s.isValid:
      scores.add: s
      validity.add 1.0
    else:
      validity.add 0.0
  let bestSize =
    if pop.fittest.isNil:
      "n/a"
    else:
      $pop.fittest.len
  checkpoint fmt"""
                        thread: {getThreadId()}
          virtual machine runs: {fnl.runs}
         total population size: {pop.len}
            average age in pop: {avg(ages)}
          validity rate in pop: {avg(validity).percent}
         program size variance: {variance(lengths)}
           average valid score: {Score avg(scores)}
             best score in pop: {Score pop.best}
          average program size: {avg(lengths)}
          size of best program: {bestSize}
         parsimony coefficient: {Score pop.pcoeff}
            insufficiency rate: {fnl.nans.mean.percent}
           semantic error rate: {fnl.errors.mean.percent}
              total cache hits: {int fnl.hits.sum}
                cache hit rate: {fnl.hits.mean.percent}
             total generations: {pop.generations}
               generation time: {Score genTime.mean} ms
                evolution time: {(getTime() - evoTime).inSeconds} sec
  """
  clearStats fnl

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

  type
    Aaargs = tuple
      tableau: Tableau
      inputs: LoonyQueue[FProg]
      outputs: LoonyQueue[FProg]

  proc worker(args: Aaargs) {.thread, gcsafe.} =
    let (tab, inputs, outputs) = args

    checkpoint "seeding in", getThreadId()

    {.gcsafe.}:
      let fnl = newFennel()
      var pop = randomPop(fnl, tab, prims)
      pop.fitness = fitness

      var leader: Hash
      var transit: FProg
      var evoTime = getTime()
      var genTime: RunningStat
      while true:
        transit = pop inputs
        if not transit.isNil:
          pop.add transit

        let clock = getTime()
        let p = generation pop
        genTime.push (getTime() - clock).inMilliseconds.float

        if not pop.fittest.isNil:
          if pop.fittest.hash != leader:
            leader = pop.fittest.hash

            for copies in 1..2:
              transit = clone pop.fittest
              transit.source = getThreadId()
              push(outputs, transit)

        if p.generation mod statFrequency == 0:
          dumpStats pop

        if tab.useParsimony and rand(10) == 0:
          discard pop.parsimony(poff)

        if pop.generations mod (statFrequency*10) == 0:
          fnl.clearCache()

  randomize()

  let
    tab =
      Tableau(seedPopulation:   1000, maxPopulation: 1000,
              maxGenerations: 500_000, seedProgramSize: 5,
              tournamentSize: 6, useParsimony: on)

  var args =
    (tableau: tab,
     inputs: newLoonyQueue[FProg](),
     outputs: newLoonyQueue[FProg]())

  var threads: seq[Thread[Aaargs]]
  newSeq(threads, countProcessors())
  for thread in threads.mitems:
    createThread(thread, worker, args)
  main(tab, args.outputs, args.inputs)
  for thread in threads.mitems:
    joinThread thread
