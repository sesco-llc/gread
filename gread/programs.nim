import std/options
import std/hashes

import gread/ast
import gread/spec

type
  Program*[T] = ref object
    core*: Option[int]        ## ideally holds the core where we were invented
    source*: int              ## usually the threadId where we were invented
    generation*: Generation   ## the generation number in which we arrived
    hash*: Hash               ## pre-generated hash for the program's ast
    score*: Score             ## the score of this program when last evaluated
    ast*: Ast[T]              ## the ast of the program itself

  Fitness*[T: ref] = proc(q: T; p: Program[T]): Score ##
  ## a function that takes your custom Language object and
  ## a suitable program as input; the result is a Score

proc zombie*(p: Program): bool {.inline.} =
  ## true if the program is invalid and may only be used for genetic data
  DeadCode in p.ast[0].flags

proc `zombie=`*(p: Program; b: bool) =
  ## mark a program as invalid; idempotent
  incl(p.ast[0].flags, DeadCode)

func len*(p: Program): int =
  ## some objective measurement of the program; ast length
  p.ast.len

proc `$`*[T](p: Program[T]): string =
  ## renders the program as raw ast
  $p.ast

proc `<`*[T](a, b: Program[T]): bool =
  ## some objective measurement of two programs; score
  a.score < b.score

proc `==`*[T](a, b: Program[T]): bool =
  ## some objective measurement of two programs; score
  a.score == b.score

proc `<=`*[T](a, b: Program[T]): bool =
  ## some objective measurement of two programs; score
  a.score < b.score or a.score == b.score

proc newProgram*[T](a: Ast[T]): Program[T] =
  ## instantiate a new program from the given ast
  Program[T](ast: a, hash: hash a, score: NaN)

proc newProgram*[T](a: Ast[T]; score: Score): Program[T] =
  Program[T](ast: a, hash: hash a, score: score)

proc clone*[T](p: Program[T]): Program[T] =
  Program[T](ast: p.ast, hash: p.hash, score: p.score, source: p.source,
             core: p.core, generation: p.generation)

proc isValid*(p: Program): bool =
  ## true if the program is valid; this will raise a defect
  ## if we have not scored the program yet
  if p.zombie:
    false
  elif p.score.isNaN:
    raise Defect.newException "score the program before measuring validity"
  else:
    true
