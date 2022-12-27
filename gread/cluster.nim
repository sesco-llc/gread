when not compileOption"threads":
  {.error: "cluster support requires threads".}

import std/atomics
import std/deques
import std/json
import std/logging
import std/options
import std/os
import std/osproc
import std/random
import std/sequtils
import std/strformat

import pkg/cps
import pkg/sysinfo
import pkg/insideout
import pkg/grok/kute

import gread/spec
import gread/programs
import gread/population
import gread/tableau
import gread/data
import gread/evolver
import gread/grammar
import gread/audit

type
  EvalResult*[T, V] = object
    program*: Program[T]
    results*: seq[Option[V]]

  ControlKind* = enum
    ckWorkerQuit

  TransportKind* = enum
    ctMetrics
    ctEvolver
    ctPopulation
    ctProgram
    ctPrograms
    ctEvalResult
    ctMessage
    ctControl

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
    of ctMessage:
      message*: string
    of ctControl:
      control*: ControlKind
      argument*: JsonNode

  TransportQ*[T, V] = Mailbox[ClusterTransport[T, V]]
  IO[T, V] = tuple[input, output: TransportQ[T, V]]

  Cluster*[T, V] = ref object
    name: string
    cores: GreadSet[CoreId]
    io: IO[T, V]                   ## how we move data between threads

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

proc push*[T, V](tq: TransportQ[T, V]; item: sink ClusterTransport[T, V]) {.inline.} =
  tq.send item

proc pop*[T, V](tq: TransportQ[T, V]): ClusterTransport[T, V] {.deprecated.} =
  discard tryRecv(tq, result)

proc recv*[T, V](tq: TransportQ[T, V]): ClusterTransport[T, V] {.deprecated.} =
  result = insideout.recv tq

proc push*[T, V](tq: TransportQ[T, V]; control: ControlKind;
                 argument: JsonNode = newJNull()) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctControl,
                                 control: control, argument: argument)

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

proc continuationRunner*(queue: Mailbox[Continuation]) {.cps: Continuation.} =
  ## continuation worker
  var work: Deque[Continuation]
  {.gcsafe.}:
    while true:
      var c: Continuation
      if work.len == 0:
        c = queue.recv()
        if dismissed c:
          break
        work.addLast c
      else:
        # trampoline that tolerates errors and coops
        var o: Continuation = work.popFirst()
        if o.running:
          try:
            o = trampoline o
            work.addLast o.Continuation
          except CatchableError as e:
            error fmt"{e.name}: {e.msg}"
            error "dismissing continuation..."

        if queue.tryRecv c:
          if dismissed c:
            break
          work.addLast c
  when defined(useMalloc) and not defined(valgrind):
    echo fmt"thread exit; {Kute memoryUsed()} of {Kute memoryArena()}"


# we'll run as many as (processors) threads, though we
# may not recommend more than (cores) concurrency
const greadCores {.intdefine.}: int = 0  ## zero means "auto-detect"
let processors* =
  when greadCores == 0:
    max(1, countProcessors())
  else:
    max(1, greadCores)

var threads*: ContinuationPool[Continuation]
var shelf*: seq[Mailbox[Continuation]]

const ContinuationRunner = whelp continuationRunner
for core in 0..<processors:
  shelf.add newMailbox[Continuation]()
  var runtime = spawn(ContinuationRunner, shelf[^1])
  when defined(greadPin):
    pinToCpu(runtime, core)
  threads.add runtime

proc sendToCore(c: Continuation; core: Natural) =
  shelf[core mod shelf.len].send c

proc initWork*[T, V](work: var Work[T, V]; tab: Tableau;
                     grammar: Grammar;
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

proc forceShare*(work: Work; program: Program) =
  ## send a better program to other threads
  var transit = program
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

iterator programs*[T, V](transport: ClusterTransport[T, V]): var Program[T] =
  ## iterate over programs passed in a transport envelope
  if transport.isNil:
    raise Defect.newException "attempt to iterate nil transport"
  else:
    case transport.kind
    of ctProgram:
      yield transport.program
    of ctPrograms:
      for program in transport.programs.mitems:
        yield program
    of ctPopulation:
      for program in transport.population.mitems:
        yield program
    else:
      raise Defect.newException "programs iterator called on " & $transport.kind

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

proc clusterSize*(work: Work): int =
  ## exposes the size of the parent cluster
  work.cluster.size

var nextId: Atomic[CoreId]         ## the value of the next core identity

proc nextCore*(): CoreId =
  ## returns a new CoreId which may be used by a cluster
  nextId.fetchAdd(1)

proc redress*[T, V](cluster: Cluster[T, V]; work: var Work[T, V]) =
  ## freshen a work object with a new core and i/o channels, etc.
  work.io = cluster.io
  work.core = some nextCore()
  work.cluster = cluster

proc initWork*[T, V](cluster: Cluster[T, V]): Work[T, V] =
  ## instantiate a new Work object which is already redress(ed)
  cluster.redress result

proc boot*[T, V](cluster: Cluster[T, V]; worker: C; core: CoreSpec) =
  ## boot a cluster with a worker continuation
  sendToCore(worker, get core)
  cluster.cores.incl(get core)

proc boot*[T, V](cluster: Cluster[T, V]; worker: C) =
  ## boot a cluster with a worker continuation
  let core = nextCore()
  sendToCore(worker, core)
  cluster.cores.incl(core)

when false:
  proc halt*(cluster: Cluster; core = none CoreId) =
    ## halt a cluster or a particular core
    for i, thread in threads.mitems:
      joinThread thread

proc newTransportQ*[T, V](): TransportQ[T, V] =
  ## create a new transport queue for messaging between threads
  newMailbox[ClusterTransport[T, V]](32768)

proc newCluster*[T, V](name = ""): Cluster[T, V] =
  ## create a new cluster
  var cores: GreadSet[CoreId]
  initGreadSet cores
  result = Cluster[T, V](name: name, cores: cores,
                         io: (input: newTransportQ[T, V](),
                              output: newTransportQ[T, V]()))

proc name*(cluster: Cluster): string =
  ## name a cluster
  if cluster.name == "":
    "gread cluster"
  else:
    cluster.name
