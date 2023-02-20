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
import std/strformat
import std/strutils

import pkg/cps
import pkg/cutelog
import pkg/grok/kute
import pkg/insideout

import gread/spec
import gread/programs
import gread/population
import gread/tableau
import gread/data
import gread/evolver
import gread/grammar
import gread/audit

const logLevel {.strdefine.} = "lvlInfo"

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
    ctPrograms
    ctEvalResult
    ctMessage
    ctControl
    ctSymbolSets

  ClusterTransport*[T, V] = ref object
    case kind*: TransportKind
    of ctMetrics:
      metrics*: PopMetrics
    of ctEvolver:
      evolver*: Evolver[T, V]
    of ctPopulation:
      population*: Population[T]
    of ctPrograms:
      programs*: seq[Program[T]]
    of ctSymbolSets:
      symbolsets*: seq[SymbolSet[T, V]]
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

  Terminator*[T; V] = proc(evo: var Evolver[T, V]): bool ##
  ## user-supplied predicate returning true when the evolver should terminate

  Work*[T, V] = object
    name*: string                          ## for reporting purposes
    core*: CoreSpec                        ## threadId-like concept
    stats*: int                            ## how often to emit stats
    rng*: Option[Rand]                     ## seeded RNG
    tableau*: Tableau
    grammar*: Grammar
    population*: Population[T]
    operators*: seq[OperatorWeight[T, V]]  ## operators & their weights
    dataset*: seq[SymbolSet[T, V]]
    fitone*: FitOne[T, V]
    fitmany*: FitMany[T, V]
    terminator*: Terminator[T, V]          ## predicate for terminating a worker
    strength*: Strength[V]                 ## compute a metric for sorting
    io*: IO[T, V]                          ## how we move data between threads
    cluster: Cluster[T, V]

proc tryPush*[T, V](tq: TransportQ[T, V]; item: sink ClusterTransport[T, V]): bool {.inline.} =
  tq.trySend item

proc push*[T, V](tq: TransportQ[T, V]; item: sink ClusterTransport[T, V]) {.inline.} =
  tq.send item

export recv

proc push*[T, V](tq: TransportQ[T, V]; control: ControlKind;
                 argument: JsonNode = nil) =
  let argument =
    if argument.isNil:
      newJNull()
    else:
      argument
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctControl,
                                 control: control, argument: argument)

proc tryPush*[T, V](tq: TransportQ[T, V]; program: sink Program[T]): bool =
  ## convenience for pushing into a transport queue
  tq.tryPush ClusterTransport[T, V](kind: ctPrograms, programs: @[program])

proc push*[T, V](tq: TransportQ[T, V]; program: sink Program[T]) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctPrograms, programs: @[program])

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

proc push*[T, V](tq: TransportQ[T, V]; dataset: sink seq[SymbolSet[T, V]]) =
  ## convenience for pushing into a transport queue
  tq.push ClusterTransport[T, V](kind: ctSymbolSets, symbolsets: dataset)

template push*(cluster: Cluster; thing: untyped): untyped =
  push(cluster.io.input, thing)

proc continuationRunner*(queue: Mailbox[Continuation]) {.cps: Continuation.} =
  ## continuation worker
  addHandler:
    newCuteConsoleLogger(fmtStr = fmt"{getThreadId()} $levelid: ", useStderr = false,
                         levelThreshold = parseEnum[logging.Level](logLevel))
  var work: Deque[Continuation]
  while true:
    var c: Continuation
    if work.len == 0:
      c = recv queue
      if dismissed c:
        break
      work.addLast c
    else:
      # trampoline that tolerates errors and coops
      var c = popFirst work
      if running c:
        try:
          work.addLast:
            bounce(move c)
        except CatchableError as e:
          error fmt"{e.name}: {e.msg}"
          error "dismissing continuation..."
      else:
        if queue.tryRecv c:
          if dismissed c:
            break
          work.addLast c
  when false and defined(useMalloc) and not defined(valgrind):
    echo fmt"thread exit; {Kute memoryUsed()} of {Kute memoryArena()}"

# we'll run as many as (processors) threads, though we
# may not recommend more than (cores) concurrency
const greadCores {.intdefine.}: int = 0  ## zero means "auto-detect"
let processors* =
  when greadCores == 0:
    max(1, countProcessors())
  else:
    max(1, greadCores)

var threads: ContinuationPool[Continuation]
var mailbox = newMailbox[Continuation]()

const ContinuationRunner = whelp continuationRunner
for core in 0..<processors:
  var runtime = spawn(ContinuationRunner, mailbox)
  when defined(greadPin):
    pinToCpu(runtime, core)
  threads.add runtime

proc initWork*[T, V](work: var Work[T, V]; tab: Tableau;
                     grammar: Grammar;
                     operators: openArray[OperatorWeight[T, V]] = @[];
                     dataset: seq[SymbolSet[T, V]] = @[];
                     fitone: FitOne[T, V] = nil; fitmany: FitMany[T, V] = nil;
                     population: Population[T] = nil; strength: Strength[V] = nil;
                     terminator: Terminator[T, V] = nil;
                     rng = none Rand; core = none int; stats = 1000; name = "") =
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
  work.terminator = terminator
  work.rng = rng
  if work.core.isNone:
    work.core = core

proc toTransit(program: Program; core: CoreSpec = none CoreId): Program =
  result = program
  result.source = getThreadId()
  result.core = core         # set the core to help define origin

proc shareOutput*(work: Work; program: Program) =
  ## blocking send of a better program to the output pipe
  var transit = program.toTransit(work.core)
  push(work.io.output, transit)

proc calculateSharing*(rng: var Rand; sharingRate: float): int =
  ## produce a count of shares from a rate
  # if the sharing rate is < 1.0, it's a weight
  if sharingRate < 1.0:
    if rng.rand(1.0) < sharingRate:
      1
    else:
      0
  # else, it's a multiplier
  else:
    int sharingRate

proc calculateSharing*(evo: var LeanEvolver): int =
  ## produce a count of shares from an evolver
  calculateSharing(evo.rng, evo.tableau.sharingRate)

proc shareInput*(work: Work; p: Program): int {.discardable.} =
  ## try to send a better program to other threads
  ## if we meet the tableau's sharing rate.
  ## returns the number of copies shared to the input pipe
  let sharing = calculateSharing(randState(), work.tableau.sharingRate)

  # share the program as widely as is requested
  for copies in 0..<max(0, sharing):
    var transit = p.toTransit(work.core)
    if tryPush(work.io.input, transit):
      inc result

iterator programs*[T, V](transport: ClusterTransport[T, V]): var Program[T] =
  ## iterate over programs passed in a transport envelope
  if transport.isNil:
    raise Defect.newException "attempt to iterate nil transport"
  else:
    case transport.kind
    of ctPrograms:
      for program in transport.programs.mitems:
        yield program
    of ctPopulation:
      for program in transport.population.mitems:
        yield program
    else:
      raise Defect.newException "programs iterator called on " & $transport.kind

proc search*[T, V](work: Work[T, V]; evo: var Evolver[T, V]) =
  ## try to get some fresh genes from another thread
  ## and add them to the supplied population
  var transport: ClusterTransport[T, V]
  if tryRecv(work.io.input, transport):
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

proc boot*[T, V](cluster: Cluster[T, V]; worker: sink Continuation) =
  ## boot a cluster with a worker continuation
  let core = nextCore()
  mailbox.send worker
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

proc raiseBadTransportKind*(kind: TransportKind) =
  raise Defect.newException:
    fmt"unsupported transport kind: {kind}"

proc raiseBadTransportKind*[T, V](transport: ClusterTransport[T, V]) =
  raiseBadTransportKind transport.kind
