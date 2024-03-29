import std/logging
import std/macros
import std/math
import std/options
import std/sequtils
import std/strformat
import std/strutils

import pkg/redis

import gread/programs
import gread/ast
import gread/grammar
import gread/genotype
import gread/population
import gread/evolver
import gread/tournament

##[

simple redis store/load for programs

]##

type
  StoreError* = object of IOError

  ScoredGenomeMapName* = distinct string
  ScoredSourceMapName* = distinct string
  ScoredMapNames = ScoredGenomeMapName or ScoredSourceMapName

proc `$`*(store: ScoredGenomeMapName): string {.borrow.}
proc `==`*(a, b: ScoredGenomeMapName): bool {.borrow.}
proc `<`*(a, b: ScoredGenomeMapName): bool {.borrow.}
proc `$`*(store: ScoredSourceMapName): string {.borrow.}
proc `==`*(a, b: ScoredSourceMapName): bool {.borrow.}
proc `<`*(a, b: ScoredSourceMapName): bool {.borrow.}

proc store*(r: var Redis; key: ScoredMapNames; program: var Program): bool =
  ## store a program to a given redis sorted set with the program's score.
  ## returns true if the program was stored.
  let data =
    when key is ScoredGenomeMapName:
      $(program.genome)
    elif key is ScoredSourceMapName:
      $program
    else:
      {.error: "not implemented".}
  program.flags.incl Cached
  result = 1 == r.zadd($key, [(data, program.score.float)], nan="-inf")

proc store*(r: var Redis; key: ScoredMapNames; programs: var openArray[Program]): BiggestInt =
  ## store programs to a given redis sorted set with the program's score.
  ## returns the number of stored programs.
  if programs.len == 0:
    return 0
  var members = newSeq[(string, float)](programs.len)
  for index, program in programs.mpairs:
    let data =
      when key is ScoredGenomeMapName:
        $(program.genome)
      elif key is ScoredSourceMapName:
        $program
      else:
        {.error: "not implemented".}
    members[index] = (data, program.score.float)
    program.flags.incl Cached
  result = r.zadd($key, members, nan="-inf")

proc store*(r: var Redis; key: ScoredMapNames; population: Population): BiggestInt =
  ## store a population to a given redis sorted set with scores.
  ## returns the number of stored programs.
  var programs = toSeq population.items
  result = store(r, key, programs)

proc unpackGenomeToProgram[T](gram: Grammar; geno: Genome): Program[T] =
  ## turn a genome into a program
  try:
    result = πMap[T](gram, geno)
    result.flags.incl Cached
  except ShortGenome:
    raise StoreError.newException:
      "deserialization failure: short genome"

proc load*(r: var Redis; key: ScoredGenomeMapName): Option[Genome] =
  ## attempt to load a random program from the sorted set
  let genes = r.zrandmember($key)
  if genes.len > 0:
    result = some genes.fromString

proc load*[T](r: var Redis; gram: Grammar; key: ScoredGenomeMapName): Option[Program[T]] =
  ## try to fetch a random program from the redis sorted set
  var genome = load(r, key)
  if genome.isSome:
    result = some unpackGenomeToProgram[T](gram, get genome)

proc clear*(r: var Redis; key: ScoredGenomeMapName; genomes: openArray[Genome]) =
  ## remove programs from the redis sorted set
  var members = newSeqOfCap[string](genomes.len)
  for genome in genomes.items:
    members.add $genome
  discard r.zrem($key, @members)

proc clear*[T](r: var Redis; key: ScoredGenomeMapName; programs: var openArray[Program[T]]) =
  ## remove programs from the redis sorted set
  var genomes = newSeqOfCap[Genome](programs.len)
  for program in programs.mitems:
    genomes.add program.genome
    program.flags.excl Cached
  clear(r, key, genomes)

proc clearWorst*(r: var Redis; key: ScoredMapNames; count: Natural = 1) =
  ## remove the `count` poorest programs from the sorted set
  discard r.zpopmin($key, count = int count)

proc randomGenomes*(r: var Redis; key: ScoredGenomeMapName;
                    count: Natural = 1): seq[Genome] =
  ## select `count` genomes at random from the sorted set
  let genes = r.zrandmembers($key, count)
  debug fmt"loaded {genes.len} random members; asked for {count}"
  setLen(result, genes.len)
  for index, gene in genes.pairs:
    result[index] = gene.fromString

proc randomPopulation*[T](r: var Redis; gram: Grammar; key: ScoredGenomeMapName;
                          size: int; core = none int): Population[T] =
  ## select a random population of programs from the sorted set
  # FIXME: also load the scores and assign them to the program
  let genes = randomGenomes(r, key, count=size)
  result = newPopulation[T](size = min(size, genes.len), core = core)
  for gene in genes.items:
    try:
      let program = unpackGenomeToProgram[T](gram, gene)
      # removed because redis cardinality matches population cardinality
      if false and program in result:
        notice fmt"rm duplicate ast {program}"
        clear(r, key, [gene])
      else:
        result.add program
    except StoreError:
      warn "ignored bad genome from " & $key

proc entirePopulation*[T](r: var Redis; gram: Grammar; key: ScoredGenomeMapName;
                          core = none int): Population[T] =
  ## select the entire population of programs from the sorted set
  var genomes = r.zrange($key, 0, -1, withScores = true)
  result = newPopulation[T](size = genomes.len, core = core)
  var program: Program[T]
  for (genome, score) in genomes.items:
    try:
      program = unpackGenomeToProgram[T](gram, genome.fromString)
      if score.isNaN:
        program.zombie = true
      else:
        program.score = score
      result.add program
    except StoreError:
      warn "ignored bad genome from " & $key

proc bestGenomes*(r: var Redis; key: ScoredGenomeMapName; count: Natural = 1): seq[Genome] =
  ## retrieve the best `count` genomes from a sorted set
  let genes = r.zrange($key, -int(count), -1)
  setLen(result, genes.len)
  for index, gene in genes.pairs:
    result[index] = gene.fromString

proc bestPrograms*[T](r: var Redis; gram: Grammar; key: ScoredGenomeMapName;
                      count: Natural = 1; core = none int): Population[T] =
  ## retrieve the `count` best programs from a sorted set
  var genomes = r.zrange($key, -int(count), -1, withScores = true)
  result = newPopulation[T](size = genomes.len, core = core)
  var program: Program[T]
  for (genome, score) in genomes.items:
    try:
      program = unpackGenomeToProgram[T](gram, genome.fromString)
      if score.isNaN:
        program.zombie = true
      else:
        program.score = score
      result.add program
    except StoreError:
      warn "ignored bad genome from " & $key

proc unpack[T](gram: Grammar; s: string): Option[Program[T]] =
  ## parse a string from redis into a program, if possible
  mixin newProgram
  const supportsParsing = compiles(newProgram "string")
  if s.len == 0:
    return none Program[T]

  var program: Program[T]
  when supportsParsing:
    # parse it using tree-sitter
    program = newProgram s
    program.flags.incl Cached
  else:
    # remove extant unsupported keys
    raise StoreError.newException:
      "deserialization failure: no tree-sitter available"
  if program.isInitialized:
    result = some program

proc load*[T](r: var Redis; gram: Grammar; key: string): Option[Program[T]] =
  ## try to fetch a random program from the redis key set
  mixin newProgram
  var s = r.srandmember(key)
  try:
    result = unpack[T](gram, s)
  except StoreError as e:
    warn e.msg
    warn "rm'ing bad redis value -- did your grammar change?"
    discard r.srem(key, s)
    result = none Program[T]

proc newPopulation*[T](r: var Redis; gram: Grammar; key: string;
                       size: int; core = none int): Population[T] =
  ## Compose a new population using random selections from redis;
  ## may result in a short population if insufficient cached data
  ## exists in redis, or if deserialization fails for key members.
  let strings = r.srandmember(key, size)
  # NOTE: create a population with the supplied size
  result = newPopulation[T](size = max(size, strings.len), core = core)
  for s in strings.items:
    try:
      let p = unpack[T](gram, s)
      if p.isSome:
        result.add get(p)
    except StoreError:
      discard

proc newPopulation*[T](r: var Redis; gram: Grammar; key: string;
                       core = none int): Population[T] =
  ## Compose a new population using all cached programs from redis;
  ## swallows any deserialization failures.
  let strings = r.smembers(key)
  result = newPopulation[T](size = strings.len, core = core)
  for s in strings.items:
    try:
      let p = unpack[T](gram, s)
      if p.isSome:
        result.add get(p)
    except StoreError:
      discard

iterator trim*[T, V](r: var Redis; evo: var Evolver[T, V]; domain: string): Program[T] =
  ## emit the worst programs until the population is within the maximum
  ## defined by the tableau; also remove programs from the cache domain
  for loser in evo.trim():
    clear(r, loser, domain)
    yield loser
