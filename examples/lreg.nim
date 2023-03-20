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
import gread/treepops
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
    if result.isNaN:
      raise Defect.newException "result is nan"
    cache[genome] = CacheNode(score: result, code: source)

proc score(genome: Genome): Option[float] =
  try:
    result = some: cache[genome].score
  except KeyError:
    result = some: computeScore(genome)
  if result.isSome and get(result).isNaN:
    raise Defect.newException "score is nan"

when isMainModule:
  import pkg/cutelog

  const logLevel {.strdefine.} = "lvlInfo"
  addHandler:
    newCuteConsoleLogger(fmtStr = "$datetime: ",
                         levelThreshold = parseEnum[logging.Level](logLevel))

  randomize(greadSeed)

  proc coop(c: Continuation): Continuation {.cpsMagic.} = c

  proc mapGenome*[T](grammar: Grammar; genome: sink Genome; truncate = false): Option[Program[T]] =
    ## map a genome to the given grammar; optionally truncate the genome
    ## before installation in the resulting Program
    try:
      let (pc, ast) = πGE[T](grammar, genome)
      if truncate:
        genome = genome[0..<pc.int]
      result = some newProgram(ast, genome)
    except ShortGenome:
      result = none Program[T]

  proc dumpGenome(genome: Genome) =
    var program = mapGenome[Fennel](gram, genome)
    if program.isSome:
      var program = get program
      program.score = computeScore(genome)
      dumpScore program

  proc myMakeRoom[T](evo: var TreeEvolver[T]) =
    while evo.population.len >= evo.tableau.maxPopulation:
      doAssert evo.tableau.tournamentSize > 1
      var genome = evo.evict()
      cache.del(genome)

  proc add[Genome](evo: var TreeEvolver[Genome]; item: sink Genome) =
    evo.myMakeRoom()
    evo.population.push(item)

  proc terminator[T](evo: var TreeEvolver[T]): bool =
    result = evo.generation >= llsMany
    when compiles(evo.fittest.isSome):
      result = result or evo.fittest.isSome and evo.fittest.get.score >= goodEnough
    result = result or (getMonoTime() - evo.birthday).inSeconds >= llsDuration
    if result:
      info fmt"terminator terminating evolver {evo.core}"

  proc dumpPopulation(evo: TreeEvolver[Genome]) =
    echo "--------------------------------------------------------------"
    for genome in evo.population.items:
      dumpGenome genome

  proc leanWorker*(args: Work[Fennel, LuaValue]) {.cps: Continuation.} =
    # you can adjust these weights to change mutation rates
    var operatorWeights = {
      operator("crossover", crossoverGroup[Genome]):                     0.5,
      operator("asymxover", asymmetricCrossoverGroup[Genome]):           0.5,
      operator(" 1% noise", geNoisy1pt0_Group[Genome]):                  0.5,
    }

    initGreadTable cache

    if fnl.isNil:
      fnl = newFennel()

    let prng =
      if args.rng.isSome:
        get args.rng
      else:
        initRand()

    var evo: TreeEvolver[Genome]
    evo.initEvolver(args.tableau, rng = prng)
    evo.operators = operatorWeights
    evo.core = args.core

    if args.population.isNil:
      while evo.population.len < args.tableau.seedPopulation:
        let genome = randomGenome(evo.rng, evo.tableau.seedProgramSize)
        doAssert genome.len > 0
        evo.add(genome)
    else:
      for program in args.population.items:
        evo.add(program.genome)

    var finest: Option[Genome]
    var gen: Generation
    let ignorePeers = args.core.get.int mod 2 != 0
    var shared: GreadSet[Hash]
    initGreadSet shared
    while gen <= args.tableau.maxGenerations:
      coop() # give other evolvers a chance

      inc gen

      if evo.calculateSharing() > 0:
        var transport: ClusterTransport[Fennel, LuaValue]
        if not ignorePeers and tryRecv(args.io.input, transport):
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
            discard args.shareInput(program)

          except ShortGenome:
            discard
          except CatchableError as e:
            echo repr(e)
            quit 1

      # update winner
      if finest.isNone or get(finest) != evo.population.best:
        var program = πMap[Fennel](gram, evo.population.best)
        program.generation = gen
        program.score = computeScore(program.genome)
        program.core = evo.core
        if not shared.containsOrIncl(program.ast.hash):
          args.shareOutput(program)
          dumpScore program
          #dumpPopulation evo.population
        finest = some program.genome

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
              discard πMap[Fennel](gram, genome)
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

    when true:
      try:
        var program = πMap[Fennel](gram, evo.population.best)
        program.source = getThreadId()
        program.core = args.core
        program.generation = gen
        program.score = computeScore(program.genome)
        dumpScore program
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
    var workerCount = cores
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
          inc winners

          if fittest.score < goodEnough:
            continue

          notice fmt"winner, winner, chicken dinner: {fittest.score}"

          once:
            for index in 1..workerCount:
              info "shutting down worker " & $index
              push(outputs, ckWorkerQuit)

      else:
        raise Defect.newException "unsupported transport: " & $transport.kind
    let secs = (getMonoTime() - evo.birthday).inMilliseconds.float / 1000.0
    notice fmt"last generation: {fittest.generation} secs: {ff(secs.float)}"
    notice fmt"number of winners: {winners} ({ff(winners.float / secs.float)}/sec)"

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
      args.core = some nextCore()
      clump.boot(whelp leanWorker(args))

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
