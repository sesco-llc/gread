when not compileOption"threads":
  {.error: "cluster support requires threads".}

import std/sequtils
import std/options
import std/osproc
import std/random

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
    cores: seq[CoreId]
    threads: seq[Thread[Work[T, V]]]
    worker: Worker[T, V]
    pq: IO[T]                      ## program gene transfer
    negs: seq[ProgramQueue[T]]     ## thread-local invalid program caches
    nextId: CoreId                 ## the value of the next core identity

  Worker*[T, V] = proc(w: Work[T, V]) {.thread.}

  Work*[T, V] = object
    core*: Option[CoreId]                  ## threadId-like concept
    stats*: int                            ## how often to emit stats
    tableau*: Tableau
    primitives*: Primitives[T]
    operators*: seq[OperatorWeight[T, V]]  ## operators & their weights
    dataset*: seq[SymbolSet[T, V]]
    targets*: Option[seq[Score]]
    fitone*: FitOne[T, V]
    fitmany*: FitMany[T, V]
    io*: IO[T]                             ## how we send/receive genes
    neg: ProgramQueue[T]                   ## receives invalid programs
    cluster: Cluster[T, V]

proc initWork*[T, V](work: var Work[T, V]; tab: Tableau;
                     primitives: Primitives[T] = nil;
                     operators: openArray[OperatorWeight[T, V]] = @[];
                     fitone: FitOne[T, V] = nil; fitmany: FitMany[T, V] = nil;
                     dataset: seq[SymbolSet[T, V]] = @[];
                     targets = none seq[Score];
                     core = none int; stats = 1000) =
  ## initialize a work object for passing setup instructions
  ## to worker threads
  work = Work[T, V](tableau: tab, primitives: primitives, dataset: dataset,
                    operators: @operators, stats: stats, core: core,
                    fitone: fitone, fitmany: fitmany)

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

proc pin*(cluster: Cluster; cores: openArray[int]) =
  ## pin a cluster to a selection of cores
  for i, thread in cluster.threads.mpairs:
    pinToCpu(thread, cores[i mod cores.len])

proc size*(cluster: Cluster): int =
  ## returns the number of evolvers in the cluster
  cluster.threads.len

proc nextId(cluster: Cluster): CoreId =
  result = cluster.nextId
  inc cluster.nextId

proc boot*[T, V](cluster: Cluster[T, V]; work: Work[T, V];
                 threads: Natural = processors) =
  ## boot a cluster with a work assignment
  var w = work
  w.io = cluster.programQueues()
  w.cluster = cluster
  for i in 1..threads:
    setLen(cluster.threads, cluster.threads.len + 1)
    cluster.negs.add newLoonyQueue[Program[T]]()
    w.neg = cluster.negs[^1]
    w.core = some cluster.nextId
    createThread(cluster.threads[^1], cluster.worker, w)

proc boot*[T, V](cluster: Cluster[T, V]; work: Work[T, V];
                 dataset: seq[SymbolSet[T, V]];
                 targets: Option[seq[Score]] = none seq[Score];
                 threads = processors) =
  ## boot a cluster with a work assignment and specific dataset/targets
  var w = work
  w.dataset = dataset
  w.targets = targets
  cluster.boot(w, threads)

proc boot*[T, V](cluster: Cluster[T, V]; work: Work[T, V];
                 data: seq[(SymbolSet[T, V], Score)];
                 threads = processors) =
  ## boot a cluster with a work assignment; unzip the data
  var w = work
  w.dataset = newSeqOfCap[SymbolSet[T, V]](data.len)
  w.targets = newSeqOfCap[Score](data.len)
  for ss, s in data.items:
    w.dataset.add ss
    w.targets.add s
  cluster.boot(w, threads)

proc halt*(cluster: Cluster; core = none CoreId) =
  ## halt a cluster or a particular core
  for i, thread in cluster.threads.mitems:
    if core.isNone or cluster.cores[i] == get core:
      joinThread cluster.threads[i]
      del(cluster.threads, i)
      del(cluster.negs, i)

proc newCluster*[T, V](worker: Worker[T, V]): Cluster[T, V] =
  ## create a new cluster
  result = Cluster[T, V](worker: worker)
  result.pq = (newLoonyQueue[Program[T]](), newLoonyQueue[Program[T]]())

proc negativeCache*(cluster: Cluster; p: Program) =
  ## inform the members of the cluster that Program `p` is invalid
  for queue in cluster.negs.items:
    queue.push(clone p)

proc negativeCache*(work: Work; p: Program) =
  ## inform the members of the cluster that Program `p` is invalid
  negativeCache(work.cluster, p)
