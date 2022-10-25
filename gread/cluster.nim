when not compileOption"threads":
  {.error: "cluster support requires threads".}

import std/deques
import std/logging
import std/options
import std/os
import std/osproc
import std/random
import std/sequtils
import std/strformat

import pkg/loony
import pkg/cps
import pkg/sysinfo

import gread/spec
import gread/programs
import gread/population
import gread/tableau
import gread/data
import gread/evolver
import gread/grammar

type
  EvalResult*[T, V] = object
    program*: Program[T]
    results*: seq[Option[V]]

  TransportKind* = enum
    ctMetrics
    ctEvolver
    ctPopulation
    ctProgram
    ctPrograms
    ctEvalResult

  ClusterTransport*[T, V] = ref object
    case kind*: TransportKind
    of ctMetrics:
      metrics*: PopMetrics
    of ctEvolver:
      evolver*: Evolver[T, V]
    of ctPopulation:
      population*: Population[T]
    of ctProgram:
      program*: Program[T]
    of ctPrograms:
      programs*: seq[Program[T]]
    of ctEvalResult:
      result*: EvalResult[T, V]

  TransportQ*[T, V] = LoonyQueue[ClusterTransport[T, V]]
  IO[T, V] = tuple[input, output: TransportQ[T, V]]

  Cluster*[T, V] = ref object
    name: string
    cores: seq[CoreId]
    io: IO[T, V]           ## how we move data between threads
    nextId: CoreId         ## the value of the next core identity

  Worker*[T, V] = proc(w: Work[T, V]) {.thread.}

  Work*[T, V] = object
    name*: string                          ## for reporting purposes
    core*: Option[CoreId]                  ## threadId-like concept
    stats*: int                            ## how often to emit stats
    rng*: Option[Rand]                     ## seeded RNG
    tableau*: Tableau
    grammar*: Grammar
    population*: Population[T]
    operators*: seq[OperatorWeight[T, V]]  ## operators & their weights
    dataset*: seq[SymbolSet[T, V]]
    targets*: Option[seq[V]]
    fitone*: FitOne[T, V]
    fitmany*: FitMany[T, V]
    strength*: Strength[V]                 ## compute a metric for sorting
    io*: IO[T, V]                          ## how we move data between threads
    cluster: Cluster[T, V]

  CQ = LoonyQueue[C]

# we'll run as many as (processors) threads, though we
# may not recommend more than (cores) concurrency
let processors* = max(1, countProcessors())
var
  threads: seq[Thread[CQ]]
  shelf: seq[CQ]

proc push*[T, V](tq: TransportQ[T, V]; program: sink Program[T]) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctProgram, program: program)

proc push*[T, V](tq: TransportQ[T, V]; population: sink Population[T]) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctPopulation, population: population)

proc push*[T, V](tq: TransportQ[T, V]; metrics: sink PopMetrics) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctMetrics, metrics: metrics)

proc push*[T, V](tq: TransportQ[T, V]; evolver: sink Evolver[T, V]) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctEvolver, evolver: evolver)

proc push*[T, V](tq: TransportQ[T, V]; programs: sink seq[Program[T]]) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctPrograms, programs: programs)

proc push*[T, V](tq: TransportQ[T, V]; evaluated: sink EvalResult[T, V]) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctEvalResult, result: evaluated)

template push*(cluster: Cluster; thing: untyped): untyped =
  push(cluster.io.input, thing)

proc continuationRunner*(queue: CQ) {.thread.} =
  ## continuation worker
  var work: Deque[C]
  {.gcsafe.}:
    while true:
      var c = pop queue
      if not c.isNil:
        work.addLast c
      if work.len == 0:
        when not defined(greadBenchmark):
          sleep 10
      else:
        # trampoline that tolerates errors and coops
        var o: Continuation = work.popFirst()
        if o.running:
          try:
            # run a single leg at a time
            o = o.fn(o)
            work.addLast o.C
          except Exception as e:
            writeStackFrames o
            stdmsg().writeLine fmt"{e.name}: {e.msg}"
            stdmsg().writeLine "dismissing continuation..."

for core in 0..<processors:
  setLen(threads, threads.len + 1)
  setLen(shelf, shelf.len + 1)
  shelf[^1] = newLoonyQueue[C]()
  createThread(threads[^1], continuationRunner, shelf[^1])
  when defined(greadPin):
    pinToCpu(threads[^1], core)

proc sendToCore(c: C; core: Natural) =
  shelf[core mod shelf.len].push c

proc initWork*[T, V](work: var Work[T, V]; tab: Tableau;
                     grammar: Grammar = nil;
                     operators: openArray[OperatorWeight[T, V]] = @[];
                     dataset: seq[SymbolSet[T, V]] = @[];
                     fitone: FitOne[T, V] = nil; fitmany: FitMany[T, V] = nil;
                     population: Population[T] = nil; strength: Strength[V] = nil;
                     targets = none seq[V]; rng = none Rand;
                     core = none int; stats = 1000; name = "") =
  ## initialize a work object for passing setup instructions to worker threads;
  ## this is now just a convenience to reduce line count
  work.tableau = tab
  work.grammar = grammar
  work.name = name
  work.strength = strength
  work.dataset = dataset
  work.operators = @operators
  work.stats = stats
  work.fitone = fitone
  work.fitmany = fitmany
  work.population = population
  work.rng = rng
  if work.core.isNone:
    work.core = core

proc forceShare*(work: Work; p: Program) =
  ## send a better program to other threads
  var transit = clone p
  transit.source = getThreadId()
  transit.core = work.core         # set the core to help define origin
  push(work.io.output, transit)

proc share*(work: Work; p: Program) =
  ## send a better program to other threads
  ## if we meet the tableau's sharing rate
  let sharing =
    # if the sharing rate is < 1.0, it's a weight
    if work.tableau.sharingRate < 1.0:
      if rand(1.0) < work.tableau.sharingRate:
        1
      else:
        0
    # else, it's a multiplier
    else:
      int work.tableau.sharingRate

  # share the program as widely as is requested
  for copies in 0..<max(0, sharing):
    forceShare(work, p)

iterator programs*[T, V](transport: ClusterTransport[T, V]): Program[T] =
  ## iterate over programs passed in a transport envelope
  if transport.isNil:
    raise Defect.newException "attempt to iterate nil transport"
  else:
    case transport.kind
    of ctProgram:
      yield transport.program
    of ctPrograms:
      for program in transport.programs.items:
        yield program
    of ctPopulation:
      for program in transport.population.items:
        yield program
    else:
      warn "programs iterator ignored " & $transport.kind

proc search*(work: Work; evo: var Evolver) =
  ## try to get some fresh genes from another thread
  ## and add them to the supplied population
  var transport = pop work.io.input
  if not transport.isNil:
    for program in programs(transport):
      evo.introduce program

proc programQueues*[T, V](cluster: Cluster[T, V]): IO[T, V] =
  ## returns input and output queues which cluster
  ## members will use to exchange novel programs
  cluster.io

proc size*(cluster: Cluster): int =
  ## returns the number of evolvers in the cluster
  cluster.cores.len

proc nextCore*(cluster: Cluster): Option[CoreId] =
  ## returns the next CoreId which will be used by the cluster
  result = some cluster.nextId
  inc cluster.nextId

proc redress*[T, V](cluster: Cluster[T, V]; work: var Work[T, V]) =
  ## freshen a work object with a new core and i/o channels, etc.
  work.io = cluster.io
  work.core = cluster.nextCore
  work.cluster = cluster

proc initWork*[T, V](cluster: Cluster[T, V]): Work[T, V] =
  ## instantiate a new Work object which is already redress(ed)
  cluster.redress result

proc boot*[T, V](cluster: Cluster[T, V]; worker: C; core: CoreSpec) =
  ## boot a cluster with a worker continuation
  sendToCore(worker, get core)
  cluster.cores.add: get core

proc halt*(cluster: Cluster; core = none CoreId) =
  ## halt a cluster or a particular core
  for i, thread in threads.mitems:
    joinThread thread

proc newTransportQ*[T, V](): TransportQ[T, V] =
  ## create a new transport queue for messaging between threads
  newLoonyQueue[ClusterTransport[T, V]]()

proc newCluster*[T, V](name = ""): Cluster[T, V] =
  ## create a new cluster
  result = Cluster[T, V](name: name,
                         io: (input: newTransportQ[T, V](),
                              output: newTransportQ[T, V]()))

proc name*(cluster: Cluster): string =
  ## name a cluster
  if cluster.name == "":
    "cores " & cluster.cores.map(`$`).join(",")
  else:
    cluster.name
