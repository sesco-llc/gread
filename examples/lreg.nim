import std/hashes
import std/logging
import std/math
import std/options
import std/os
import std/random
import std/sets
import std/strformat
import std/strutils
import std/times

import gread
import gread/fennel except variance
import gread/genotype
import gread/aliasmethod
import gread/heapqueue

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

proc add[T](population: var HeapQueue[T]; size: Natural; rng: var Rand; item: sink T) =
  # XXX: nim's capacity(seq) does not work
  while population.len >= size:
    remove(rng, population, max(1, tab.tournamentSize))
  population.push(item)

proc add[T](population: var HeapQueue[T]; rng: var Rand; item: sink T) =
  # XXX: nim's capacity(seq) does not work
  population.add(tab.maxPopulation, rng, item)

proc computeScore[T](fnl: Fennel; genome: T): float =
  var results: array[data.len, float]
  try:
    var p = πMap[Fennel](gram, genome)
    #var results = newSeq[float](dataset.len)
    block complete:
      for index, locals in dataset.pairs:
        let s = evaluate(fnl, p, locals)
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

when isMainModule:
  import pkg/cutelog

  const logLevel {.strdefine.} = "lvlInfo"
  addHandler:
    newCuteConsoleLogger(fmtStr = "$datetime: ",
                         levelThreshold = parseEnum[logging.Level](logLevel))

  randomize()

  proc coop(c: C): C {.cpsMagic.} = c

  proc newHeapPopulation[T](gram: Grammar; size: int; core = none CoreId): HeapQueue[T] =
    var fnl = newFennel()
    var cache: GreadCache[T, float]
    initGreadCache(cache, size * 10)  # FIXME

    proc score(genome: T): float =
      try:
        result = cache[genome]
      except KeyError:
        result = computeScore(fnl, genome)
        cache[genome] = result

        # clear the VM periodically
        if fnl.runs mod 500_000 == 0:
          fnl.tidyVM()

    proc worseThan(a, b: T): bool =
      let x = a.score
      let y = b.score
      x < y

    result = initHeapQueue[Genome](worseThan, initialSize = size)

  proc leanWorker*(args: Work[Fennel, LuaValue]) {.cps: C.} =
    template maxPop: int = args.tableau.maxPopulation
    var rng =
      if args.rng.isSome:
        get args.rng
      else:
        initRand()

    # lightweight operators
    var operators: AliasMethod[GenomeOperator[Genome]]
    initAliasMethod(operators, operatorWeights)

    var population = newHeapPopulation[Genome](args.grammar, maxPop, args.core)
    if args.population.isNil:
      while population.len < args.tableau.seedPopulation:
        let genome = randomGenome(rng, args.tableau.seedProgramSize)
        assert genome.len > 0
        population.push(genome)
    else:
      for program in args.population.items:
        population.add(maxPop, rng, program.genome)

    var evoTime = getTime()
    var finest: Option[Genome]
    var gen: Generation
    while gen <= args.tableau.maxGenerations:
      coop() # give other evolvers a chance

      inc gen
      when true:
        if gen mod args.stats == 0:
          echo args.core, " ", gen

      if rng.rand(1.0) < args.tableau.sharingRate:
        if rng.rand(1.0) < 0.51:
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
                  population.add(maxPop, rng, program.genome)
        else:
          when true:
            let stale = population[rng.rand(population.high)]
            try:
              var program = πMap[Fennel](gram, stale)
              program.source = getThreadId()
              program.core = args.core
              program.generation = gen
              var message = ClusterTransport[Fennel, LuaValue](kind: ctProgram, program: program)
              discard trySend(args.io.input, message)
            except ShortGenome:
              discard
            except CatchableError as e:
              echo repr(e)
              quit 1

          when true:
            if finest.isNone or get(finest) != population[population.high]:
              try:
                var program = πMap[Fennel](gram, population[population.high])
                if finest.isNone or program.genome != get finest:
                  finest = some program.genome
                program.source = getThreadId()
                program.core = args.core
                program.generation = gen
                warn $program
                push(args.io.output, program)
              except ShortGenome:
                discard
              except CatchableError as e:
                echo repr(e)
                quit 1

      # lean generational loop
      while true:
        let operator = choose(operators, rng)
        var discoveries = 0
        try:
          for genome in operator(rng, population, args.tableau.tournamentSize):
            inc discoveries
            population.add(maxPop, rng, genome)
          break
        except ShortGenome:
          if discoveries > 0:
            break
        except CatchableError as e:
          echo repr(e)
          quit 1

    when false:
      block:
        let shared = newPopulation[Fennel](size = population.len)
        while population.len > 0:
          try:
            var program = πMap[Fennel](gram, pop population)
            program.source = getThreadId()
            program.core = args.core
            program.generation = gen
            shared.add program
          except CatchableError:
            discard
        push(args.io.output, shared)

    when true:
      try:
        var program = πMap[Fennel](gram, population[population.high])
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

    let et = getTime()
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
    notice fmt"last generation: {fittest.generation} secs: {ff(secs.float / 1000.0)}"
    #notice fmt"number of winners: {winners} ({ff(winners.float / (secs.float / 1000.0))}/sec)"

  # each worker gets a Work object as input to its thread
  let clump = newCluster[Fennel, LuaValue]()
  var args = clump.initWork()
  initWork(args, tab, grammar = gram,
           dataset = dataset, fitone = fitone, fitmany = fitmany,
           strength = fennel.strength, stats = statFrequency)

  for core in 1..cores:
    when greadSeed == 0:
      args.rng = some: initRand()
    else:
      args.rng = some: initRand(greadSeed)
    if cores == 1:
      args.tableau.sharingRate = 0.0
    clump.boot(whelp leanWorker(args), args.core)
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
