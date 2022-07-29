import std/strutils
import std/options

import pkg/frosty/streams as brrr
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

const
  frostyMagic = "brrr 2"

type
  StoreError* = object of IOError

proc store*[T](r: var Redis; p: Program[T]; key: string) =
  ## store a program to a given redis key set
  var geno = freeze p.genome
  geno.add frostyMagic
  p.flags.incl Cached
  discard r.sadd(key, geno)   # return value is number of elements added

proc unpack[T](gram: Grammar; s: string): Option[Program[T]] =
  ## parse a string from redis into a program, if possible
  mixin newProgram
  const supportsParsing = compiles(newProgram "string")
  if s.len == 0:
    return none Program[T]

  try:
    var p: Program[T]
    if s.endsWith frostyMagic:
      # it's serialized via frosty
      var s = s
      setLen(s, s.len - frostyMagic.len)  # remove the magic
      var geno: Genome
      thaw(s, geno)
      try:
        var (pc {.used.}, ast) = πGE[T](gram, geno)
        p = newProgram(ast, geno)
      except ShortGenome:
        raise StoreError.newException:
          "deserialization failure: short genome"
    else:
      when supportsParsing:
        # parse it using tree-sitter
        p = newProgram s
      else:
        # remove extant unsupported keys
        raise StoreError.newException:
          "deserialization failure: no tree-sitter available"
    if not p.isNil:
      p.flags.incl Cached
      result = some p
  except ThawError as e:
    raise StoreError.newException "deserialization failure: " & e.msg

proc clear*[T](r: var Redis; p: Program[T]; key: string) =
  ## remove a program from the redis key set
  var geno = freeze p.genome
  geno.add frostyMagic
  p.flags.excl Cached
  discard r.srem(key, geno)

proc load*[T](r: var Redis; gram: Grammar; key: string): Option[Program[T]] =
  ## try to fetch a random program from the redis key set
  mixin newProgram
  var s = r.srandmember(key)
  if s.len < frostyMagic.len:
    result = none Program[T]
  else:
    try:
      result = unpack[T](gram, s)
    except StoreError as e:
      echo e.msg
      echo "rm'ing bad redis value -- did your grammar change?"
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

iterator trim[T, V](r: var Redis; evo: var Evolver[T, V]; domain: string): Program[T] =
  ## emit the worst programs until the population is within the maximum
  ## defined by the tableau; also remove programs from the cache domain
  for loser in evo.trim():
    echo "clear: " & $loser
    clear(r, loser, domain)
    yield loser
