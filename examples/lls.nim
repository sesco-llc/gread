when not compileOption"threads":
  {.error: "needs --threads:on".}

import std/hashes
import std/logging
import std/options
import std/os
import std/osproc
import std/packedsets
import std/random
import std/sets
import std/strformat
import std/strutils
import std/times

import gread
import gread/fennel except variance

import pkg/sysinfo
import pkg/cps
import pkg/lunacy

include preamble

let
  cores =
    when not defined(release) and greadSeed != 0:
      1
    else:
      processors
      #max(1, getNumTotalCores())

var gram: Grammar
initFennelGrammar(gram, llsGrammar)

# you can adjust these weights to change mutation rates
let operators = {
  geCrossover[Fennel, LuaValue]:        1.0,
  #geMutation[Fennel, LuaValue]:         1.0,
  #subtreeXover[Fennel, LuaValue]:       1.0,
  #randomSubtreeXover[Fennel, LuaValue]: 1.0,
  #randomCrossover[Fennel, LuaValue]:    1.0,
  #asymmetricCrossover[Fennel, LuaValue]:        6.0,
  #randomAsymmetricCrossover[Genome]:        1.0,
  geNoise1pt0[Fennel, LuaValue]:        1.0,
  geNoise2pt0[Fennel, LuaValue]:        1.0,
  geNoise4pt0[Fennel, LuaValue]:        1.0,
}

const
  # given a line of (x, y) points, solve for y given x
  data = @[(1,6), (2,5), (3,7), (4,10)]

# preparing the data for use in the fitness()
var dataset: seq[Locals]
for (x, y) in data.items:
  dataset.add:
    initLocals [("x", x.toLuaValue), ("y", y.toLuaValue)]

proc fitone(fnl: Fennel; locals: Locals; p: var FProg): Option[LuaValue] =
  ## given a datapoint, run the program and return the residual
  let s = evaluate(fnl, p, locals)
  if s.isValid:
    result =
      some:
        toLuaValue -abs(locals["y"].toFloat - s.toFloat)

proc fitmany(fnl: Fennel; iter: iterator(): (ptr Locals, ptr LuaValue);
             p: FProg): Option[LuaValue] =
  ## given several residuals, return the sum of squares
  var results = newSeqOfCap[float](data.len)
  for locals, s in iter():
    if s[].isValid:
      results.add s[]
    else:
      return none LuaValue
  if results.len > 0:
    let s = toLuaValue -ss(results)
    if s.isValid:
      result = some s

when isMainModule:
  import pkg/cutelog

  import gread/cluster

  const logLevel {.strdefine.} = "lvlInfo"
  addHandler:
    newCuteConsoleLogger(fmtStr = "$datetime: ",
                         levelThreshold = parseEnum[logging.Level](logLevel))

  randomize()

  # the main loop monitors inventions
  proc main(work: Work; inputs, outputs: TransportQ[Fennel, LuaValue]) =
    # create a population to monitor new inventions
    let fnl = newFennel()
    var workerCount = work.clusterSize
    var monitor = tab
    monitor.maxPopulation = 2
    var evo: Evolver[Fennel, LuaValue]
    initEvolver(evo, fnl, monitor)
    evo.strength = strength
    evo.grammar = gram
    evo.operators = operators
    evo.dataset = dataset
    evo.fitone = fitone
    evo.fitmany = fitmany
    evo.population =
      newPopulation[Fennel](monitor.maxPopulation, core = evo.core)

    let et = getTime()
    var fittest: Program[Fennel]
    var winners = 0
    while true:
      let transport = recv inputs
      case transport.kind
      of ctControl:
        case transport.control
        of ckWorkerQuit:
          dec workerCount
          debug fmt"cluster has {workerCount} workers"
          if workerCount == 0:
            break
      of ctPopulation:
        # thread shut-down
        discard
      of ctProgram:
        var p = transport.program
        #if cores > 1:
        #  push(outputs, p)

        p.score = Score NaN
        evo.makeRoom()
        evo.add p

        if fittest == get(evo.fittest):
          continue

        fittest = get(evo.fittest)
        dumpScore fittest
        inc winners

        if fittest.score < goodEnough:
          continue

        notice fmt"winner, winner, chicken dinner: {fittest.score}"

        once:
          for index in 1..workerCount:
            debug "shutting down worker " & $index
            push(outputs, ckWorkerQuit)

      else:
        raise Defect.newException "unsupported transport: " & $transport.kind
    let secs = (getTime() - et).inMilliseconds
    notice fmt"last generation: {fittest.generation} secs: {(getTime() - et).inSeconds}"
    #notice fmt"number of winners: {winners} ({ff(winners.float / (secs.float / 1000.0))}/sec)"

  # each worker gets a Work object as input to its thread
  let clump = newCluster[Fennel, LuaValue]()
  var args = clump.initWork()
  initWork(args, tab, grammar = gram, operators = operators,
           dataset = dataset, fitone = fitone, fitmany = fitmany,
           strength = fennel.strength, stats = statFrequency)

  for core in 1..cores:
    when greadSeed == 0:
      args.rng = some: initRand()
    else:
      args.rng = some: initRand(greadSeed)
    if cores == 1:
      args.tableau.sharingRate = 0.0
    clump.boot(whelp worker(args), args.core)
    clump.redress args

  # run the main loop to gatekeep inventions
  let (inputs, outputs) = clump.programQueues()
  main(args, outputs, inputs)
