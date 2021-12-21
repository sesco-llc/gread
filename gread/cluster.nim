when not compileOption"threads":
  {.error: "cluster support requires threads".}

import std/sequtils
import std/options
import std/osproc
import std/random
import std/os
import std/strformat
import std/deques

import pkg/loony
import pkg/cps

import gread/spec
import gread/programs
import gread/population
import gread/tableau
import gread/data
import gread/evolver
import gread/grammar

type
  ProgramQueue*[T] = LoonyQueue[Program[T]]
  IO[T] = tuple[inputs, outputs: ProgramQueue[T]]
  Cluster*[T, V] = ref object
    name: string
    cores: seq[CoreId]
    pq: IO[T]                      ## program gene transfer
    negs: seq[ProgramQueue[T]]     ## thread-local invalid program caches
    nextId: CoreId                 ## the value of the next core identity

  Worker*[T, V] = proc(w: Work[T, V]) {.thread.}

  Work*[T, V] = object
    core*: Option[CoreId]                  ## threadId-like concept
    stats*: int                            ## how often to emit stats
    tableau*: Tableau
    grammar*: Grammar[T]
    operators*: seq[OperatorWeight[T, V]]  ## operators & their weights
    dataset*: seq[SymbolSet[T, V]]
    targets*: Option[seq[V]]
    fitone*: FitOne[T, V]
    fitmany*: FitMany[T, V]
    io*: IO[T]                             ## how we send/receive genes
    neg: ProgramQueue[T]                   ## receives invalid programs
    cluster: Cluster[T, V]

  CQ = LoonyQueue[C]

let processors = countProcessors()
var
  threads: seq[Thread[CQ]]
  shelf: seq[CQ]

proc corker*(queue: CQ) {.thread.} =
  var work: Deque[C]
  {.gcsafe.}:
    while true:
      var c = pop queue
      if not c.isNil:
        work.addLast c
      if work.len == 0:
        when not defined(greadBenchmark):
          sleep 2000
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
  createThread(threads[^1], corker, shelf[^1])
  pinToCpu(threads[^1], core)

proc sendToCore(c: C; core: Natural) =
  shelf[core mod shelf.len].push c

proc initWork*[T, V](work: var Work[T, V]; tab: Tableau;
                     grammar: Grammar[T] = nil;
                     operators: openArray[OperatorWeight[T, V]] = @[];
                     dataset: seq[SymbolSet[T, V]] = @[];
                     fitone: FitOne[T, V] = nil; fitmany: FitMany[T, V] = nil;
                     targets = none seq[V];
                     core = none int; stats = 1000) =
  ## initialize a work object for passing setup instructions to worker threads;
  ## this is now just a convenience to reduce line count
  work.tableau = tab
  work.grammar = grammar
  work.dataset = dataset
  work.operators = @operators
  work.stats = stats
  work.fitone = fitone
  work.fitmany = fitmany
  if work.core.isNone:
    work.core = core

proc share*(work: Work; p: Program) =
  ## send a better program to other threads
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
  for copies in 0..max(0, sharing):
    var transit = clone p
    transit.source = getThreadId()
    push(work.io.outputs, transit)

proc search*(work: Work; population: Population) =
  ## try to get some fresh genes from another thread
  ## and add them to the supplied population
  var transit = pop work.io.inputs
  if not transit.isNil:
    when true:
      population.introduce transit    # no propogation of winners into fittest
    else:
      population.add transit          # allows a winner to further propogate

iterator invalidPrograms*[T, V](work: Work[T, V]): Program[T] =
  ## iterate over, and remove, programs marked invalid elsewhere
  while not work.neg.isNil:
    var transit = pop work.neg
    if transit.isNil:
      break
    yield transit

proc programQueues*[T, V](cluster: Cluster[T, V]): IO[T] =
  ## returns input and output queues which cluster
  ## members will use to exchange novel programs
  (cluster.pq.inputs, cluster.pq.outputs)

proc size*(cluster: Cluster): int =
  ## returns the number of evolvers in the cluster
  cluster.threads.len

proc nextCore*(cluster: Cluster): Option[CoreId] =
  ## returns the next CoreId which will be used by the cluster
  result = some cluster.nextId
  inc cluster.nextId

proc redress*[T, V](cluster: Cluster[T, V]; work: var Work[T, V]) =
  ## freshen a work object with a new core and i/o channels, etc.
  cluster.negs.add newLoonyQueue[Program[T]]()
  work.neg = cluster.negs[^1]
  work.io = cluster.programQueues()
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
    if true: #core.isNone or cluster.cores[i] == get core:
      joinThread thread
      #del(cluster.threads, i)
      #del(cluster.negs, i)
  cluster.negs = @[]

proc newCluster*[T, V](name = ""): Cluster[T, V] =
  ## create a new cluster
  result = Cluster[T, V](name: name)
  result.pq = (newLoonyQueue[Program[T]](), newLoonyQueue[Program[T]]())

proc name*(cluster: Cluster): string =
  if cluster.name == "":
    "cores " & cluster.cores.map(`$`).join(",")
  else:
    cluster.name

proc negativeCache*(cluster: Cluster; p: Program) =
  ## inform the members of the cluster that Program `p` is invalid
  for queue in cluster.negs.items:
    queue.push(clone p)

proc negativeCache*(work: Work; p: Program) =
  ## inform the members of the cluster that Program `p` is invalid
  negativeCache(work.cluster, p)
