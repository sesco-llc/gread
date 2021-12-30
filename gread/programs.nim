import std/options
import std/hashes

import pkg/adix/stat
import pkg/adix/lptabz

#from pkg/frosty import frostyError, FreezeError, ThawError

import gread/ast
import gread/genotype
import gread/spec

const
  programCache* = false

type
  ProgramFlag* = enum
    FinestKnown

  #Program*[T] = ref object
  ProgramObj[T] = object
    genome: Genome            ## the genome used to construct the program
    code: Option[string]      ## cache of the rendered source code
    core*: Option[int]        ## ideally holds the core where we were invented
    runtime*: MovingStat[float32, uint32]  ## tracks the runtime for this program
    source*: int              ## usually the threadId where we were invented
    generation*: Generation   ## the generation number in which we arrived
    hash*: Hash               ## pre-generated hash for the program's ast
    score*: Score             ## the score of this program when last evaluated
    flags*: set[ProgramFlag]  ## flag enums associated with the program
    ast*: Ast[T]              ## the ast of the program itself
    scores*: MovingStat[float64, uint32] ## statistics around valid scores
    when programCache:
      cache: LPTab[Hash, Option[Score]] ## cache of score given symbol set hash
  Program*[T] = ref ProgramObj[T]

proc push*(p: Program; s: Score) =
  ## record a valid score for statistics purposes
  p.scores.push s

proc genome*(p: Program): Genome {.inline.} =
  ## the program's source genome
  p.genome

proc zombie*(p: Program): bool {.inline.} =
  ## true if the program is invalid and may only be used for genetic data
  DeadCode in p.ast[0].flags

proc `zombie=`*(p: Program; b: bool) =
  ## mark a program as invalid; idempotent
  if b:
    incl(p.ast[0].flags, DeadCode)
  elif p.zombie:
    raise Defect.newException "the undead must never live again"

func len*(p: Program): int =
  ## some objective measurement of the program; ast length
  p.ast.len

proc render*(p: Program): string =
  mixin render
  if p.code.isSome:
    result = get p.code
  else:
    result = render p.ast
    p.code = some result

proc `$`*(p: Program): string =
  ## renders the program as source code if possible; else raw ast
  if p.code.isSome:
    p.code.get
  else:
    render p

proc `<`*[T](a, b: Program[T]): bool =
  ## some objective measurement of two programs; score
  a.score < b.score

proc `==`*[T](a, b: Program[T]): bool =
  ## some objective measurement of two programs; score
  # this silliness works around a nim bug with our
  # `==`() leaking into system/arc's reference counting
  if a.isNil != b.isNil:
    false
  elif a.isNil:
    true
  else:
    a.score == b.score

proc `<=`*[T](a, b: Program[T]): bool =
  ## some objective measurement of two programs; score
  a.score < b.score or a.score == b.score

proc newProgram*[T](a: Ast[T]): Program[T] =
  ## instantiate a new program from the given ast
  result = Program[T](ast: a, hash: hash a, score: NaN)
  when programCache:
    init(result.cache, initialSize = 2)

proc newProgram*[T](a: Ast[T]; geno: Genome): Program[T] =
  ## instantiate a new program from the given ast and genome
  result = newProgram(a)
  result.genome = geno

proc clone*[T](p: Program[T]): Program[T] =
  ## it's not a clone if it's different
  result =
    Program[T](ast: p.ast, hash: p.hash, score: p.score, source: p.source,
               code: p.code, flags: p.flags, core: p.core, genome: p.genome,
               scores: p.scores, generation: p.generation)
  when programCache:
    init(result.cache, initialSize = 2)

proc isValid*(p: Program): bool =
  ## true if the program is known to yield valid output; this will raise
  ## a defect if we have not scored the program yet
  if p.zombie:
    false
  else:
    p.score.isValid

proc addScoreToCache*(p: Program; h: Hash; s: Option[Score]) {.deprecated.} =
  ## record the score for a given input hash
  when programCache:
    if not p.zombie:
      p.cache[h] = s

proc getScoreFromCache*(p: Program; h: Hash): Option[Score] {.deprecated.} =
  ## attempt to retrieve the cached score for a given hash of the input
  # FIXME: use withValue when cb fixes adix
  when programCache:
    if not p.zombie:
      if h in p.cache:
        result = p.cache[h]

proc cacheSize*(p: Program): int {.deprecated.} =
  ## the size of a program's score cache
  when programCache:
    result = p.cache.len

when false:
  proc serialize*[S, T](output: var S; input: ProgramObj[T]) =
    serialize(output, input.ast)
    serialize(output, input.genome)
    serialize(output, input.flags)

  proc deserialize*[S, T](input: var S; output: var ProgramObj[T]) =
    var ast: Ast[T]
    deserialize(input, ast)
    output = newProgram(ast)
    deserialize(input, output.genome)
    deserialize(input, output.flags)
