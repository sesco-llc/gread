import std/algorithm
import std/deques
import std/hashes
import std/math
import std/options
import std/packedsets
import std/random
import std/sequtils

import pkg/adix/stat except variance, Option

import gread/spec
import gread/programs
import gread/maths

{.experimental: "strictFuncs".}

const
  defaultParsimony = 0.01
  #[

  this is quite a bit slower when enabled, but that's probably because
  the population is not swamped with programs that are comprised of just
  a single terminal, etc. the greater average program size is what makes
  larger populations more valuable.

  ]#
  populationCache = true

type
  PopMetrics* = object
    core*: CoreSpec
    generation*: Generation
    lengths*: MovingStat[float32, uint32]       ## accurate only after resetMetrics()
    scores*: MovingStat[float64, uint32]
    ages*: MovingStat[float32, uint32]
    immigrants*: int
    inventions*: int
    leaders*: int
    zombies*: int
    parsimony*: float
    validity*: MovingStat[float32, uint32]
    caches*: MovingStat[float32, uint32]
    # the rest are populated JIT
    bestSize*: int
    bestGen*: Generation
    bestScore*: Score
    staleness*: float
    usurper*: CoreSpec
    size*: int

  Population*[T] = ref object
    programs: seq[Program[T]]
    ken*: PopMetrics
    when populationCache:
      cache*: GreadSet[Hash]

  PopLike*[T] = concept c
    c[int] is Program[T]
    (c[int] < c[int]) is bool
    c.len is int
    for v in c:
      v is Program[T]

  LegacyPop[T] = Population[T] or PopLike[T]

# make sure we aren't breaking concepts 🙄
assert seq[Program[int]] is PopLike[int]
assert array[5, Program[int]] is PopLike[int]

proc parsimony*(ken: PopMetrics; pop: Population): float

template learn(pop: Population; p: Program; pos: int) =
  when populationCache:
    pop.cache.incl p.hash
  if p.isValid:
    if p.score.isValid:
      pop.ken.scores.push p.score
    else:
      raise Defect.newException "program is valid, score isn't"
    pop.ken.validity.push 1.0
  else:
    pop.ken.validity.push 0.0
  pop.ken.lengths.push p.len.float
  if p.core == pop.ken.core:
    pop.ken.ages.push float(int p.generation)

template forget(population: Population; program: Program; pos: int) =
  when populationCache:
    population.cache.excl program.hash
  if program.isValid:
    if program.score.isValid:
      population.ken.scores.pop program.score
    population.ken.validity.pop 1.0
  else:
    population.ken.validity.pop 0.0
  population.ken.lengths.pop program.len.float
  if program.core == population.ken.core:
    population.ken.ages.pop float(int program.generation)

template withInitialized*(pop: Population; logic: untyped): untyped =
  ## execute the body only when the population is initialized
  if pop.isNil:
    raise Defect.newException "initialize your population first"
  else:
    logic

template withPopulated*(pop: Population; logic: untyped): untyped =
  ## execute the body only when the population has members
  withInitialized pop:
    if pop.len == 0:
      raise ValueError.newException "population is empty"
    else:
      logic

proc newPopulation*[T](size = 0; core = none int): Population[T] =
  ## create a new population with the given capacity and core
  result = Population[T](programs: newSeqOfCap[Program[T]](size))
  result.ken.core = core
  result.ken.parsimony = NaN
  when populationCache:
    initGreadSet result.cache

func len*[T](p: Population[T]): int =
  ## the number of programs in the population
  p.programs.len

proc rescale(ken: PopMetrics; score: Score): Score =
  ## rescale a given score according to the distribution of the population
  result =
    Score:
      sgn(score).float *
        abs(score.float / min(score.float, ken.scores.min.float))

proc penalizeSize(ken: PopMetrics; score: Score; length: int): Score =
  ## apply some pressure on program size
  if ken.parsimony.isNaN:
    result = score
  else:
    var s = rescale(ken, score).float
    if ken.parsimony < 0.0:  # length appears to be hurting scores
      # reduce the score of longer programs
      s += ken.parsimony * length.float
    else:                        # length appears to be helping scores
      # raise the score of longer programs
      s += ken.parsimony * length.float
    result = Score s

proc score*(ken: PopMetrics; score: Score; length: int): Score =
  ## adjust the score according to the population's parsimony and a length
  if ken.parsimony.isNaN:
    score
  elif score.isValid:
    penalizeSize(ken, score, length)
  else:
    Score NaN

proc score*(ken: PopMetrics; p: Program): Score =
  ## retrieve the parsimonious Score for Program `p`
  if ken.parsimony.isNaN:
    p.score
  elif p.isValid:
    penalizeSize(ken, p.score, p.len)
  else:
    Score NaN

proc contains*(population: Population; program: Program): bool =
  ## membership test for programs
  when populationCache:
    program.hash in population.cache
  else:
    for member in population.programs.items:
      if member.hash == program.hash:
        return true

template addImpl[T](population: Population[T]; p: Program[T]) =
  population.programs.add p
  learn(population, p, population.programs.high)

proc introduce*[T](population: Population[T]; p: Program[T]) =
  ## introduce a foreign program to the local pop
  withInitialized population:
    when populationCache:
      if not population.cache.containsOrIncl p.hash:
        addImpl(population, p)
    else:
      addImpl(population, p)

proc add*[T](pop: Population[T]; p: Program[T]) =
  ## add a new program to the population
  withInitialized pop:
    if not p.isInitialized:
      raise Defect.newException "nil program"
    elif pop.isNil:
      raise Defect.newException "nil pop"
    else:
      template optimizeAway: bool =
        when populationCache:
          pop.cache.containsOrIncl(p.hash)
        else:
          false
      if not optimizeAway():
        addImpl(pop, p)

iterator items*[T](pop: Population[T]): Program[T] =
  withInitialized pop:
    for p in pop.programs.items:
      yield p

iterator mitems*[T](pop: Population[T]): var Program[T] =
  withInitialized pop:
    for p in pop.programs.mitems:
      yield p

iterator pairs*[T](pop: Population[T]): (int, Program[T]) =
  var i: int
  for p in pop.items:
    yield (i, p)
    inc i

type
  IndexedProgram[T] = tuple
    index: int
    program: Program[T]

proc `[]`*[T](population: Population[T]; index: int): var Program[T] =
  ## retrieve a program via (unstable?) index
  population.programs[index]

proc high*(population: Population): int =
  ## retrieve the highest index of a population
  population.programs.high

proc randomMember*[T](population: Population[T];
                      rng: var Rand): IndexedProgram[T] =
  ## return a random member of the population
  withPopulated population:
    let index = rng.rand population.programs.high
    result = (index: index, program: population.programs[index])

proc del*[T](population: Population[T]; index: int) =
  ## remove a specific member of the population
  withPopulated population:
    population.forget(population.programs[index], index)
    del(population.programs, index)

proc pop*[T](population: Population[T]): Program[T] =
  ## remove and return a member of the population;
  ## this does not reset population metrics
  withPopulated population:
    let index = population.programs.high
    result = population.programs[index]
    del(population, index)

proc randomRemoval*[T](population: Population[T]; rng: var Rand): Program[T] =
  ## remove and return a random member of the population;
  ## this does not reset population metrics
  withPopulated population:
    let query = population.randomMember(rng)
    result = query.program
    del(population, query.index)

proc parsimony*(population: Population): float =
  ## compute parsimony for valid members of the population
  withInitialized population:
    var scores = newSeqOfCap[float](population.len)
    var lengths = newSeqOfCap[float](population.len)
    for index, program in population.pairs:
      if program.isValid:
        scores.add program.score.float
        lengths.add program.len.float
    result = covariance(lengths, scores) / variance(lengths)

proc parsimony*(ken: PopMetrics; pop: Population): float =
  ## compute parsimony for members of the population with valid scores
  withInitialized pop:
    if ken.scores.n == 0:
      return defaultParsimony
    var scores = newSeqOfCap[float](ken.scores.n)
    var lengths = newSeqOfCap[float](ken.scores.n)
    for i, p in pop.pairs:
      if p.isValid:
        scores.add rescale(ken, p.score)
        lengths.add p.len.float
    result = covariance(lengths, scores) / variance(lengths)

proc nextGeneration*(ken: var PopMetrics): Generation =
  ## inform the population that we're entering a new generation
  inc ken.generation
  result = ken.generation

proc scoreChanged*(pop: Population; p: var Program; s: Option[float]; index: int) =
  ## inform the population of a change to the score of `p` at `index`; this
  ## is used to update metrics and parsimony
  withInitialized pop:
    when false:
      if p.isValid:
        pop.ken.validity.pop 1.0
        if p.score.isValid:
          if pop.ken.scores.n == 0:
            raise Defect.newException "fewer than zero scores?"
          pop.ken.scores.pop p.score
      else:
        pop.ken.validity.pop 0.0
      if s.isSome and s.get.isValid:
        p.score = get s
        pop.ken.validity.push 1.0
        pop.ken.scores.push p.score
        p.zombie = false  # NOTE: trigger a defect if necessary
      else:
        p.score = NaN
        p.zombie = true
        pop.ken.validity.push 0.0
    else:
      if s.isSome and get(s).isValid:
        p.score = get s
        p.zombie = false  # NOTE: trigger a defect if necessary
      else:
        p.score = NaN
        p.zombie = true

func paintMetrics*(metrics: var PopMetrics; population: Population) =
  ## reset validity, score, and parsimony metrics in the population; O(n)
  withInitialized population:
    clear metrics.validity
    clear metrics.scores
    clear metrics.caches
    clear metrics.lengths
    metrics.size = 0
    for program in population.items:
      inc metrics.size
      if program.isValid:
        metrics.validity.push 1.0
        if program.score.isValid:
          metrics.scores.push program.score
      else:
        metrics.validity.push 0.0
      metrics.lengths.push program.len.float
      if program.core == metrics.core:
        metrics.ages.push float(int program.generation)

func paintFittest*(metrics: var PopMetrics; fittest: Program) =
  metrics.bestSize = fittest.len
  metrics.bestScore = fittest.score
  metrics.bestGen = fittest.generation
  if fittest.core == metrics.core:
    let current = metrics.generation.int.float
    metrics.staleness = fittest.generation.float / current
    metrics.usurper = none CoreId
  else:
    metrics.staleness = NaN
    metrics.usurper = fittest.core

func metrics*(population: Population): PopMetrics =
  ## returns a copy of the population's metrics
  result = population.ken
  result.paintMetrics(population)

func clone*[T](population: Population[T]; core = none CoreId): Population[T] =
  ## create a copy of the population
  result = newPopulation[T](size = population.programs.len, core = core)
  for program in population.items:
    result.add program
  paintMetrics(result.ken, result)

proc sort*(population: Population; order = SortOrder.Ascending) =
  sort(population.programs, order = order)

proc randomRemoval*[T](q: var PopLike[T]; rng: var Rand): T =
  let index = rng.rand(q.high)
  result = q[index]
  q.del(index)
