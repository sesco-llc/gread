import std/options

import pkg/frosty/streams as brrr
import pkg/redis

import gread/programs
import gread/ast
import gread/grammar
import gread/genotype

##[

simple redis store/load for programs

]##

const
  frostyMagic = "brrr 2"

proc store*[T](r: var Redis; p: Program[T]; key: string) =
  ## store a program to a given redis key set
  var geno = freeze p.genome
  geno.add frostyMagic
  discard r.sadd(key, geno)

proc load*[T](r: var Redis; gram: Grammar[T]; key: string): Option[Program[T]] =
  ## try to fetch a random program from the redis key set
  mixin newProgram
  const supportsParsing = compiles(newProgram "string")
  var s = r.srandmember(key)
  if s.len == 0:
    return none Program[T]

  try:
    var p: Program[T]
    if s.endsWith frostyMagic:
      # it's serialized via frosty
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
        discard r.srem(key, s)
    if not p.isNil:
      p.ast[0].flags.incl Cached
      result = some p
  except ThawError:
    # deserialization failure; remove it
    discard r.srem(key, s)
