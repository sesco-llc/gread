import std/options

import pkg/frosty/streams as brrr
import pkg/redis

import gread/programs
import gread/ast

##[

simple redis store/load for programs

]##

const
  frostyMagic = "brrr"

proc store*[T](p: Program[T]; r: var Redis; domain: string) =
  ## store a program to a given redis key set
  var ast = freeze p.ast
  ast.add frostyMagic
  discard r.sadd(domain, ast)

proc load*[T](r: var Redis; key: string): Option[Program[T]] =
  ## try to fetch a random program from the redis key set
  mixin newProgram
  var s = r.srandmember(key)
  if s.len == 0:
    return none Program[T]

  try:
    var p: Program[T]
    if s.endsWith frostyMagic:
      # it's serialized via frosty
      setLen(s, s.len - frostyMagic.len)  # remove the magic
      var ast: Ast[T]
      thaw(s, ast)
      p = newProgram ast
    elif compiles(newProgram s):
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
