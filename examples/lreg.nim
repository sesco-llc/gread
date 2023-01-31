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
import gread/heappops

import pkg/cps
import pkg/lunacy
import pkg/insideout

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
let operatorWeights = {
  geCrossover[Genome]:        1.0,
  #asymmetricCrossover[Genome]:        6.0,
  #randomAsymmetricCrossover[Genome]:        1.0,

  geNoise1pt0[Genome]:        1.0,
  geNoise2pt0[Genome]:        1.0,
  geNoise4pt0[Genome]:        1.0,
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
        compileFennel(fnl, render p)
    #var results = newSeq[float](dataset.len)
    block complete:
      for index, locals in dataset.pairs:
        let s = evaluateLua(fnl, source, locals)
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

proc worseThan(a, b: Genome): bool =
  let x = a.score
  let y = b.score
  x < y

proc newGenomeHeap(initialSize: Natural): HeapPop[Genome] =
  initHeapPop(worseThan, initialSize = initialSize)

when isMainModule:
  import pkg/cutelog

  const logLevel {.strdefine.} = "lvlInfo"
  addHandler:
    newCuteConsoleLogger(fmtStr = "$datetime: ",
                         levelThreshold = parseEnum[logging.Level](logLevel))

  randomize()

  proc coop(c: C): C {.cpsMagic.} = c

  proc pop*[T](rng: var Rand; population: var HeapPop[T]; size: int): T =
    if population.len < 1:
      raise ValueError.newException:
        "cannot run a tournament with empty population"
    if size < 1:
      raise ValueError.newException:
        "cannot run a tournament with less than one competitor"
    let index = tournament(rng, population.high, size, order = Ascending)
    result = population[index]
    population.del(index)

  proc makeRoom[T](evo: var HeapEvolver[T]) =
    while evo.population.len >= evo.tableau.maxPopulation:
      var genome = pop(evo.rng, evo.population, max(1, evo.tableau.tournamentSize))
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

  proc leanWorker*(args: Work[Fennel, LuaValue]) {.cps: C.} =
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
        evo.makeRoom()
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
                  info "terminating on request"
                  break
              else:
                for program in programs(transport):
                  evo.makeRoom()
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
      while true:
        let operator = evo.chooseOperator()
        var discoveries = 0
        try:
          for genome in operator(evo.rng, evo.population, args.tableau.tournamentSize):
            inc discoveries
            evo.makeRoom()
            evo.add(genome)
          break
        except ShortGenome:
          if discoveries > 0:
            break
        except CatchableError as e:
          echo repr(e)
          quit 1

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
    let secs = (getMonoTime() - evo.birthday).inMilliseconds.float / 1000.0
    notice fmt"last generation: {fittest.generation} secs: {ff(secs.float)}"
    #notice fmt"number of winners: {winners} ({ff(winners.float / (secs.float / 1000.0))}/sec)"

  # each worker gets a Work object as input to its thread
  let clump = newCluster[Fennel, LuaValue]()
  var args = clump.initWork()
  initWork(args, tab, grammar = gram, #terminator = terminator,
           dataset = dataset, fitone = fitone, fitmany = fitmany,
           strength = fennel.strength, stats = statFrequency)

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
  import gread/audit
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
