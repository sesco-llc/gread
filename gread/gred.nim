import std/strutils
import std/options

import pkg/frosty/streams as brrr
import pkg/redis

import gread/programs
import gread/ast
import gread/grammar
import gread/genotype
import gread/population

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
  discard r.sadd(key, geno)

proc unpack[T](gram: Grammar[T]; s: string): Option[Program[T]] =
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
      var (pc, ast) = gram.πGE(geno)
      p = newProgram(ast, geno)
    else:
      when supportsParsing:
        # parse it using tree-sitter
        p = newProgram s
      else:
        # remove extant unsupported keys
        raise StoreError.newException:
          "deserialization failure: no tree-sitter available"
    if not p.isNil:
      p.ast[0].flags.incl Cached
      result = some p
  except ThawError as e:
    raise StoreError.newException "deserialization failure: " & e.msg

template removeFaulty[T](r: var Redis; gram: Grammar[T];
                         key, s: string): Option[Program[T]] =
  try:
    unpack(gram, s)
  except StoreError:
    discard r.srem(key, s)
    none Program[T]

proc load*[T](r: var Redis; gram: Grammar[T]; key: string): Option[Program[T]] =
  ## try to fetch a random program from the redis key set
  mixin newProgram
  var s = r.srandmember(key)
  result = removeFaulty(r, gram, key, s)

proc newPopulation*[T](r: var Redis; gram: Grammar[T]; key: string;
                       size: int; core = none int): Population[T] =
  ## Compose a new population using random selections from redis;
  ## may result in a short population if insufficient cached data
  ## exists in redis, or if deserialization fails for key members.
  let strings = r.srandmember(key, size)
  echo "got ", strings.len, " for query ", size, " on key ", key
  # NOTE: create a population with the supplied size
  result = newPopulation[T](size = size, core = core)
  for s in strings.items:
    let p = unpack(gram, s)
    if p.isSome:
      result.add get(p)

proc newPopulation*[T](r: var Redis; gram: Grammar[T]; key: string;
                       core = none int): Population[T] =
  ## Compose a new population using all cached programs from redis;
  ## swallows any deserialization failures.
  let strings = r.smembers(key)
  result = newPopulation[T](size = strings.len, core = core)
  for s in strings.items:
    let p = unpack(gram, s)
    if p.isSome:
      result.add get(p)
