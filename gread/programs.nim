import std/hashes
import std/math
import std/options

import pkg/adix/stat except Option
import pkg/adix/lptabz

#from pkg/frosty import frostyError, FreezeError, ThawError

import gread/ast
import gread/genotype
import gread/spec

type
  ProgramFlag* = enum
    FinestKnown
    Cached
    DeadCode
    Rendered

  Program*[T] = object
    genome: Genome            ## the genome used to construct the program
    code: Option[string]      ## cache of the rendered source code
    core*: Option[int]        ## ideally holds the core where we were invented
    runtime: MovingStat[float32, uint32]  ##
    ## tracks the runtime for this program
    source*: int              ## usually the threadId where we were invented
    generation*: Generation   ## the generation number in which we arrived
    hash: Hash                ## pre-generated hash for the program
    score*: Score             ## the score of this program when last evaluated
    flags*: set[ProgramFlag]  ## flag enums associated with the program
    ast*: Ast[T]              ## the ast of the program itself
    scores*: MovingStat[float64, uint32] ## statistics around valid scores

func isInitialized*(program: Program): bool {.inline.} =
  program.hash != default Hash

template isNil*(program: Program): bool {.deprecated: "use isInitialized/1".} =
  not program.isInitialized

proc pushRuntime*(p: var Program; nanoseconds: float) =
  p.runtime.push nanoseconds

proc runtime*(p: Program): MovingStat[float32, uint32] =
  p.runtime

proc push*(p: var Program; s: Score) =
  ## record a valid score for statistics purposes
  p.scores.push s

proc genome*(p: Program): Genome {.inline.} =
  ## the program's source genome
  p.genome

proc zombie*(p: Program): bool {.inline.} =
  ## true if the program is invalid and may only be used for genetic data
  DeadCode in p.flags

proc `zombie=`*(p: var Program; b: bool) =
  ## mark a program as invalid; idempotent
  if b:
    incl(p.flags, DeadCode)
  elif DeadCode in p.flags:
    raise Defect.newException "the undead must never live again"

func len*(p: Program): int =
  ## some objective measurement of the program; genome length
  p.genome.len

proc render*(p: var Program): string =
  mixin render
  if Rendered in p.flags or p.code.isSome:
    result = get p.code
  else:
    result = render p.ast
    p.code = some result
    p.flags.incl Rendered

proc render*(p: Program): string =
  mixin render
  if Rendered in p.flags or p.code.isSome:
    result = get p.code
  else:
    raise Defect.newException "immutable program cannot be rendered"

proc `$`*(p: var Program): string =
  ## renders the program as source code if possible; else raw ast
  render p

proc `$`*(p: Program): string =
  ## renders the program as source code if possible; else raw ast
  render p

proc `<`*[T](a, b: Program[T]): bool =
  ## some objective measurement of two programs; score
  if a.score.isNaN and not b.score.isNaN:
    true
  else:
    a.score < b.score

# trust no one
assert not(NaN < -Inf) and not(NaN > -Inf)
assert not(NaN < -0.0) and not(NaN > 0.0)
assert not almostEqual(NaN, NaN)

proc `==`*[T](a, b: Program[T]): bool =
  ## some objective measurement of two programs; score
  # this silliness works around a nim bug with our
  # `==`() leaking into system/arc's reference counting
  if a.isInitialized != b.isInitialized:
    false
  elif not a.isInitialized:
    true
  else:
    a.hash == b.hash

proc `<=`*[T](a, b: Program[T]): bool =
  ## a < b or a == b
  a < b or a == b

proc initProgram*[T](result: var Program[T]; ast: Ast[T]; genome: Genome) =
  ## initialize a new program from the given ast and genome
  result.score = Score NaN
  result.genome = genome
  result.ast = ast
  result.hash =
    if result.genome == EmptyGenome:
      hash result.ast
    else:
      hash result.genome

proc newProgram*[T](ast: Ast[T]; genome: Genome): Program[T] =
  ## instantiate a new program from the given ast and genome
  result.initProgram(ast, genome)

proc newProgram*[T](ast: Ast[T]): Program[T] =
  ## instantiate a new program from the given ast
  result.initProgram(ast, EmptyGenome)

proc isValid*(p: Program): bool =
  ## true if the program is known to yield valid output; this will raise
  ## a defect if we have not scored the program yet
  if not p.isInitialized:
    raise ValueError.newException "caught an uninitialized program"
  elif p.zombie:
    false
  else:
    p.score.isValid

func hash*(p: Program): Hash {.inline.} =
  ## hash() symbol for table purposes
  p.hash

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
