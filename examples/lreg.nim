import std/hashes
import std/logging
import std/math
import std/monotimes
import std/options
import std/os
import std/random
import std/sets
import std/strformat
import std/strutils
import std/tables
import std/times

import gread
import gread/fennel except variance
import gread/genotype
import gread/aliasmethod
import gread/audit
import gread/heappops
import gread/crossover
import gread/mutation

import pkg/cps
import pkg/lunacy
import pkg/insideout
import pkg/insideout/coz

include preamble

type
  CacheNode = object
    score: float
    code: string

#
# we'll close over these threadvars to simplify impl
#
var cache {.threadVar.}: GreadTable[Genome, CacheNode]
var fnl {.threadVar.}: Fennel

proc computeScore(genome: Genome): float =
  mixin render
  var source: string
  var results: array[data.len, float]
  try:
    var p = πMap[Fennel](gram, genome)
    source =
      if genome in cache:
        cache[genome].code
      else:
        fnl.compileChunk:
          fnl.compileFennel:
            render p
    block complete:
      for index, locals in dataset.pairs:
        let s = evalChunk(fnl, source, locals)
        if s.isValid:
          results[index] = -abs(data[index][1].float - s.toFloat)
        else:
          result = -Inf
          # might set zombie here if it'd help
          break complete
      result = -ss(results)
      if result.isNaN:
        result = -Inf
  except ShortGenome:
    result = -Inf
  except CatchableError as e:
    echo repr(e)
    quit 1
  finally:
    cache[genome] = CacheNode(score: result, code: source)

proc score(genome: Genome): float =
  try:
    result = cache[genome].score
  except KeyError:
    result = computeScore(genome)

proc betterThan(a, b: Genome): bool =
  let x = a.score
  let y = b.score
  x > y

proc newGenomeHeap(initialSize: Natural): HeapPop[Genome] =
  initHeapPop(betterThan, initialSize = initialSize)

when isMainModule:
  import pkg/cutelog

  const logLevel {.strdefine.} = "lvlInfo"
  addHandler:
    newCuteConsoleLogger(fmtStr = "$datetime: ",
                         levelThreshold = parseEnum[logging.Level](logLevel))

  randomize()

  proc coop(c: Continuation): Continuation {.cpsMagic.} = c

  proc makeRoom[T](evo: var HeapEvolver[T]) =
    while evo.population.len >= evo.tableau.maxPopulation:
      var genome = evict(evo.rng, evo.population, max(1, evo.tableau.tournamentSize))
      cache.del(genome)

  proc add[T](evo: var HeapEvolver[T]; item: sink T) =
    evo.makeRoom()
    evo.population.push(item)

  proc terminator[T](evo: var HeapEvolver[T]): bool =
    result = evo.generation >= llsMany
    when compiles(evo.fittest.isSome):
      result = result or evo.fittest.isSome and evo.fittest.get.score >= goodEnough
    result = result or (getMonoTime() - evo.birthday).inSeconds >= llsDuration
    if result:
      info fmt"terminator terminating evolver {evo.core}"

  proc leanWorker*(args: Work[Fennel, LuaValue]) {.cps: Continuation.} =
    # you can adjust these weights to change mutation rates
    var operatorWeights = {
      operator("crossover", crossoverGroup[Genome]):              300.0,
      #operator("asymmetric", asymmetricCrossoverGroup[Genome]):   100.0,
      #operator("random asym", randomAsymmetricCrossoverGroup[Genome]):   100.0,
      operator("1% noise", geNoisy1pt0_Group[Genome]):            100.0,
      operator("2% noise", geNoisy2pt0_Group[Genome]):            100.0,
      operator("4% noise", geNoisy4pt0_Group[Genome]):            100.0,
    }

    initGreadTable cache

    if fnl.isNil:
      fnl = newFennel()

    let prng =
      if args.rng.isSome:
        get args.rng
      else:
        initRand()

    var evo: HeapEvolver[Genome]
    evo.initEvolver(args.tableau, rng = prng)
    evo.operators = operatorWeights
    evo.population = newGenomeHeap(evo.tableau.maxPopulation)

    if args.population.isNil:
      while evo.population.len < args.tableau.seedPopulation:
        let genome = randomGenome(evo.rng, evo.tableau.seedProgramSize)
        assert genome.len > 0
        evo.population.push(genome)
    else:
      for program in args.population.items:
        evo.add(program.genome)

    var finest: Option[Genome]
    var gen: Generation
    while gen <= args.tableau.maxGenerations:
      coop() # give other evolvers a chance

      inc gen
      when true:
        if gen mod args.stats == 0:
          echo args.core, " ", gen

      if evo.rng.rand(1.0) < args.tableau.sharingRate:
        if evo.rng.rand(1.0) < 0.60:
          when true:
            var transport: ClusterTransport[Fennel, LuaValue]
            if tryRecv(args.io.input, transport):
              case transport.kind
              of ctControl:
                case transport.control
                of ckWorkerQuit:
                  break
              else:
                for program in programs(transport):
                  evo.add(program.genome)
        else:
          try:
            let stale = evo.randomMember()
            var program = πMap[Fennel](gram, stale)
            program.generation = gen
            if args.shareInput(program) == 0:
              if finest.isNone or get(finest) != evo.population.best:
                var program = πMap[Fennel](gram, evo.population.best)
                program.generation = gen
                args.shareOutput(program)
                finest = some program.genome
          except ShortGenome:
            discard
          except CatchableError as e:
            echo repr(e)
            quit 1

      # lean generational loop
      var discoveries = 0
      while discoveries == 0:
        let op = evo.chooseOperator()
        let group = evo.run(op)
        for genome in group.items:
          inc discoveries
          when greadReportOperators:
            let fine = evo.population.best
            evo.add(genome)
            if fine != evo.population.best:
              inc op.winners
            try:
              var program = πMap[Fennel](gram, genome)
              op.stat.push 1.0
            except ShortGenome:
              op.stat.push -1.0
          else:
            evo.add(genome)

      progress "loop"

      # terminate the evolver according to a supplied predicate
      if terminator evo:
        debug "terminator() killed evolver"
        break

    when false:
      block:
        let shared = newPopulation[Fennel](size = population.len)
        while population.len > 0:
          try:
            var program = πMap[Fennel](gram, pop evo.population)
            program.source = getThreadId()
            program.core = args.core
            program.generation = gen
            shared.add program
          except CatchableError:
            discard
        push(args.io.output, shared)

    when true:
      try:
        var program = πMap[Fennel](gram, evo.population.best)
        program.source = getThreadId()
        program.core = args.core
        program.generation = gen
        push(args.io.output, program)
      except ShortGenome:
        discard
      except CatchableError as e:
        echo repr(e)
        quit 1

    push(args.io.output, ckWorkerQuit)

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
    evo.dataset = dataset
    evo.fitone = fitone
    evo.fitmany = fitmany
    evo.population =
      newPopulation[Fennel](monitor.maxPopulation, core = evo.core)

    var fittest: Program[Fennel]
    var winners = 0
    while true:
      var transport = recv inputs
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
        echo "\nthread shutdown:"
        var i = 0
        for program in transport.population.mitems:
          echo i
          inc i
          let s = evo.score program
          if s.isSome:
            program.score = toFloat(get s)
          dumpScore program
        echo ""

      of ctPrograms:
        for p in programs(transport):
          p.score = Score NaN
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
    let secs = (getMonoTime() - evo.birthday).inMilliseconds.float / 1000.0
    notice fmt"last generation: {fittest.generation} secs: {ff(secs.float)}"
    #notice fmt"number of winners: {winners} ({ff(winners.float / (secs.float / 1000.0))}/sec)"

  # each worker gets a Work object as input to its thread
  let clump = newCluster[Fennel, LuaValue]()
  var args = clump.initWork()
  initWork(args, tab, grammar = gram, #terminator = terminator,
           dataset = dataset, fitone = fitone, fitmany = fitmany,
           strength = fennel.strength, stats = statFrequency)

  when insideoutCoz:
    when greadSeed == 0:
      args.rng = some: initRand()
    else:
      args.rng = some: initRand(greadSeed)
    leanWorker(args)
    quit 0
  else:
    for core in 1..cores:
      when greadSeed == 0:
        args.rng = some: initRand()
      else:
        args.rng = some: initRand(greadSeed)
      if cores == 1:
        args.tableau.sharingRate = 0.0
      clump.boot(whelp leanWorker(args))
      clump.redress args

  # run the main loop to gatekeep inventions
  let (inputs, outputs) = clump.programQueues()
  main(args, outputs, inputs)
  import grok/resources
  import grok/kute
  var process: ProcessResources
  sample process
  echo """
    voluntary context switches: {process.voluntaryContextSwitches}""".fmt
  echo """
  involuntary context switches: {process.involuntaryContextSwitches}""".fmt
  echo """
                process memory: {Kute process.maxResidentBytes} ({processors} threads)""".fmt
  echo """
                   memory used: {Kute memoryUsed()} of {Kute memoryArena()}""".fmt
