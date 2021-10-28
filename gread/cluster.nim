when not compileOption"threads":
  {.error: "cluster support requires threads".}

import std/options
import std/osproc

import pkg/loony

import gread/spec
import gread/primitives
import gread/programs
import gread/population
import gread/tableau
import gread/data
import gread/evolver

let processors = countProcessors()

type
  ProgramQueue*[T] = LoonyQueue[Program[T]]
  IO[T] = tuple[inputs, outputs: ProgramQueue[T]]
  Cluster*[T, V] = ref object
    threads: seq[Thread[Work[T, V]]]
    worker: Worker[T, V]
    pq: IO[T]                      ## program gene transfer
    neg: seq[ProgramQueue[T]]      ## thread-local invalid program caches

  Worker*[T, V] = proc(w: Work[T, V]) {.thread.}

  Work*[T, V] = object
    core*: Option[int]                             ## threadId-like concept
    stats*: int                                    ## how often to emit stats
    sharing*: int                                  ## share with N peers
    tableau*: Tableau
    primitives*: Primitives[T]
    operators*: seq[OperatorWeight[T, V]]          ## operators & their weights
    dataset*: seq[SymbolSet[T, V]]
    targets*: Option[seq[Score]]
    fitone*: FitOne[T, V]
    fitmany*: FitMany[T, V]
    io*: IO[T]                                     ## how we send/receive genes
    neg: ProgramQueue[T]                           ## receives invalid programs
    cluster: Cluster[T, V]

proc initWork*[T, V](work: var Work[T, V]; tab: Tableau;
                     primitives: Primitives[T] = nil; core = none int;
                     dataset: seq[SymbolSet[T, V]] = @[];
                     targets = none seq[Score];
                     fitone: FitOne[T, V] = nil; fitmany: FitMany[T, V] = nil;
                     operators: openArray[OperatorWeight[T, V]] = @[];
                     sharing = 2; stats = 1000) =
  ## initialize a work object for passing setup instructions
  ## to worker threads
  work = Work[T, V](tableau: tab, primitives: primitives, dataset: dataset,
                    operators: @operators, sharing: sharing, stats: stats,
                    core: core, fitone: fitone, fitmany: fitmany)

proc share*(work: Work; p: Program) =
  ## send a better program to other threads
  for copies in 1..max(1, work.sharing):
    var transit = clone p
    transit.source = getThreadId()
    push(work.io.outputs, transit)

proc search*(work: Work; population: Population) =
  ## try to get some fresh genes from another thread
  var transit = pop work.io.inputs
  if not transit.isNil:
    when true:
      population.introduce transit    # no propogation of winners into fittest
    else:
      population.add transit          # allows a winner to further propogate

iterator invalidPrograms*[T, V](work: Work[T, V]): Program[T] =
  ## iterate over, and remove, programs marked invalid elsewhere
  while true:
    var transit = pop work.neg
    if transit.isNil:
      break
    yield transit

proc programQueues*[T, V](cluster: Cluster[T, V]): IO[T] =
  ## returns input and output queues which cluster
  ## members will use to exchange novel programs
  (cluster.pq.inputs, cluster.pq.outputs)

proc boot*[T, V](cluster: Cluster[T, V]; work: Work[T, V]) =
  ## boot a cluster with a work assignment
  setLen(cluster.neg, cluster.threads.len)
  for i, thread in cluster.threads.mpairs:
    cluster.neg[i] = newLoonyQueue[Program[T]]()
    var w = work
    w.core = some i
    w.neg = cluster.neg[i]
    w.io = cluster.programQueues()
    w.cluster = cluster
    createThread(thread, cluster.worker, w)

proc halt*(cluster: Cluster) =
  ## shutdown a cluster
  for thread in cluster.threads.mitems:
    joinThread thread

proc pin*(cluster: Cluster; cores: openArray[int]) =
  ## pin a cluster to a selection of cores
  for i, thread in cluster.threads.mpairs:
    pinToCpu(thread, cores[i mod cores.len])

proc newCluster*[T, V](worker: Worker[T, V];
                       threads = processors): Cluster[T, V] =
  ## create a new cluster
  result = Cluster[T, V](worker: worker)
  setLen(result.threads, threads)  # work around generics issue
  result.pq = (newLoonyQueue[Program[T]](), newLoonyQueue[Program[T]]())

proc negativeCache*(cluster: Cluster; p: Program) =
  ## inform the members of the cluster that Program `p` is invalid
  for queue in cluster.neg.items:
    queue.push(clone p)

proc negativeCache*(work: Work; p: Program) =
  ## inform the members of the cluster that Program `p` is invalid
  negativeCache(work.cluster, p)
