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
import gread/generation
import gread/data

let processors = countProcessors()

type
  ProgramQueue*[T] = LoonyQueue[Program[T]]
  IO[T] = tuple[inputs, outputs: ProgramQueue[T]]
  Cluster*[T] = ref object
    threads: seq[Thread[Work[T]]]
    worker: Worker[T]
    pq: IO[T]                      ## program gene transfer
    neg: seq[ProgramQueue[T]]      ## thread-local invalid program caches

  Worker*[T] = proc(w: Work[T]) {.thread.}

  Work*[T] = object
    core*: Option[int]                             ## threadId-like concept
    stats*: int                                    ## how often to emit stats
    sharing*: int                                  ## share with N peers
    fitness*: Fitness[T]
    tableau*: Tableau
    primitives*: Primitives[T]
    operators*: seq[OperatorWeight[T]]             ## operators & their weights
    io*: IO[T]                                     ## how we send/receive genes
    neg: ProgramQueue[T]                           ## receives invalid programs
    cluster: Cluster[T]

  FitOne[T: ref; V] = proc(q: T; s: SymbolSet[T, V]; p: Program[T]): Score ##
  ## a fitness function that runs against a single symbolset

  FitMany[T: ref; V] = proc(q: T; ss: openArray[SymbolSet[T, V]];
                            p: Program[T]): Score ##
  ## a fitness function that runs against a series of symbolsets

proc initWork*[T](tab: Tableau; prims: Primitives[T];
                  ops: openArray[OperatorWeight[T]] = @[];
                  fitness: Fitness[T] = nil;
                  sharing = 2; stats = 1000; core = none int): Work[T] =
  ## create a work object suitable for passing setup instructions
  ## to worker threads
  Work[T](tableau: tab, primitives: prims, stats: stats, core: core,
          operators: @ops, fitness: fitness, sharing: sharing)

proc share*[T](work: Work[T]; p: Program[T]) =
  ## send a better program to other threads
  for copies in 1..max(1, work.sharing):
    var transit = clone p
    transit.source = getThreadId()
    push(work.io.outputs, transit)

proc search*[T](work: Work[T]; population: var Population) =
  ## try to get some fresh genes from another thread
  var transit = pop work.io.inputs
  if not transit.isNil:
    transit.score = NaN
    discard population.score transit
    when false:
      population.introduce transit    # no propogation of winners into fittest
    else:
      population.add transit          # allows a winner to further propogate

iterator invalidPrograms*[T](work: Work[T]): Program[T] =
  ## iterate over, and remove, programs marked invalid elsewhere
  while true:
    var transit = pop work.neg
    if transit.isNil:
      break
    yield transit

proc programQueues*[T](cluster: Cluster[T]): IO[T] =
  ## returns input and output queues which cluster
  ## members will use to exchange novel programs
  (cluster.pq.inputs, cluster.pq.outputs)

proc boot*[T](cluster: Cluster[T]; work: Work[T]) =
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

proc halt*[T](cluster: Cluster[T]) =
  ## shutdown a cluster
  for thread in cluster.threads.mitems:
    joinThread thread

proc pin*[T](cluster: Cluster[T]; cores: openArray[int]) =
  ## pin a cluster to a selection of cores
  for i, thread in cluster.threads.mpairs:
    pinToCpu(thread, cores[i mod cores.len])

proc newCluster*[T](worker: Worker[T]; threads = processors): Cluster[T] =
  ## create a new cluster
  result = Cluster[T](worker: worker)
  setLen(result.threads, threads)  # work around generics issue
  result.pq = (newLoonyQueue[Program[T]](), newLoonyQueue[Program[T]]())

proc negativeCache*[T](cluster: Cluster[T]; p: Program[T]) =
  ## inform the members of the cluster that Program `p` is invalid
  for queue in cluster.neg.items:
    queue.push(clone p)

proc negativeCache*[T](work: Work[T]; p: Program[T]) =
  ## inform the members of the cluster that Program `p` is invalid
  negativeCache(work.cluster, p)
