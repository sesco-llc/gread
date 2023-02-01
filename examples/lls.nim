when not compileOption"threads":
  {.error: "needs --threads:on".}

import std/hashes
import std/logging
import std/monotimes
import std/options
import std/os
import std/packedsets
import std/random
import std/sets
import std/strformat
import std/strutils
import std/times

import gread
import gread/fennel except variance

import pkg/cps
import pkg/lunacy

include preamble

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

proc terminator(evo: var Evolver[Fennel, LuaValue]): bool =
  result = evo.generation >= llsMany
  when compiles(evo.fittest.isSome):
    result = result or evo.fittest.isSome and evo.fittest.get.score >= goodEnough
  result = result or (getMonoTime() - evo.birthday).inMilliseconds >= evo.tableau.maxDurationInMs
  if result:
    info fmt"terminator terminating evolver {evo.core}"

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
      of ctPrograms:
        for p in programs(transport):
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
        raiseBadTransportKind transport

    let secs = (getMonoTime() - evo.birthday).inMilliseconds.float / 1000.0
    notice fmt"last generation: {fittest.generation} secs: {ff(secs.float)}"
    #notice fmt"number of winners: {winners} ({ff(winners.float / secs.float)}/sec)"

  # set the maximum evolver duration
  tab.maxDurationInMs = llsDuration * 1_000

  # each worker gets a Work object as input to its thread
  let clump = newCluster[Fennel, LuaValue]()
  var args = clump.initWork()
  initWork(args, tab, grammar = gram, operators = operators,
           dataset = dataset, fitone = fitone, fitmany = fitmany,
           terminator = terminator, strength = fennel.strength,
           stats = statFrequency)

  for core in 1..cores:
    when greadSeed == 0:
      args.rng = some: initRand()
    else:
      args.rng = some: initRand(greadSeed)
    if cores == 1:
      args.tableau.sharingRate = 0.0
    clump.boot(whelp worker(args))
    clump.redress args

  # run the main loop to gatekeep inventions
  let (inputs, outputs) = clump.programQueues()
  main(args, outputs, inputs)
