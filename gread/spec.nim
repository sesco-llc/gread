import std/math
import std/strutils
import std/options
import std/hashes

import gread/ast

type
  Primitives*[T] = ref object
    functions*: seq[Function[T]]
    constants*: seq[Terminal[T]]
    inputs*: seq[Terminal[T]]
    outputs*: seq[Terminal[T]]

  Program*[T] = ref object
    source*: int
    generation*: int
    hash*: Hash
    score*: Score
    zombie*: bool
    ast*: Ast[T]

  Score* = distinct float
  Fitness*[T: ref] = proc(q: T; p: Program[T]): Score

converter toFloat*(s: Score): float = float s
converter toScore*[T: float or float64](f: T): Score = Score f

proc `+`*(a, b: Score): Score {.borrow.}
proc `-`*(a, b: Score): Score {.borrow.}
proc `*`*(a, b: Score): Score {.borrow.}
proc `/`*(a, b: Score): Score {.borrow.}
proc `$`*(s: Score): string =
  float(s).formatFloat(format = ffDecimal, precision = 4)

proc percent*(f: float): string =
  (f * 100.0).formatFloat(format = ffDecimal, precision = 2) & "%"

proc newPrimitives*[T](): Primitives[T] =
  new Primitives[T]

func terminals*[T](c: Primitives[T]): seq[Terminal[T]] =
  c.constants & c.inputs & c.outputs

func len*(p: Program): int = p.ast.len

template auditLength*[T](p: Program[T]) = auditLength p.ast

proc `$`*[T](p: Program[T]): string =
  auditLength p
  $p.ast

proc `<`*[T](a, b: Program[T]): bool =
  a.score < b.score

proc `==`*[T](a, b: Program[T]): bool =
  a.score == b.score

proc `<=`*[T](a, b: Program[T]): bool =
  a.score < b.score or a.score == b.score

proc newProgram*[T](a: Ast[T]): Program[T] =
  Program[T](ast: a, hash: hash a, score: NaN)

proc newProgram*[T](a: Ast[T]; score: Score): Program[T] =
  Program[T](ast: a, hash: hash a, score: score)

proc clone*[T](p: Program[T]): Program[T] =
  Program[T](ast: p.ast, hash: p.hash, score: p.score, source: p.source,
             zombie: p.zombie, generation: p.generation)

proc avg*[T: SomeOrdinal](a: openArray[T]): T {.inline.} =
  sum(a) div T(a.len)

proc avg*[T: not SomeOrdinal](a: openArray[T]): T {.inline.} =
  sum(a) / T(a.len)

proc covariance*(a, b: openArray[float]): float =
  if a.len == b.len:
    let a1 = avg a
    let b1 = avg b
    for index in 0..<a.len:
      result += (a[index] - a1) * (b[index] - b1)
  else:
    raise ValueError.newException "inputs have unequal girth"

proc variance*[T](a: openArray[T]): T =
  let mean = avg a
  for item in a.items:
    result += (item - mean) * (item - mean)
  when T is SomeOrdinal:
    result = result div T(a.len)
  else:
    result = result / T(a.len)

proc stddev*(a: openArray[float]): float {.inline.} =
  sqrt variance(a)

proc correlation*(a, b: openArray[float]): float {.inline.} =
  covariance(a, b) / (stddev(a) * stddev(b))

proc isValid*(s: Score): bool =
  ## the score is a value we can get useful comparisons from
  if s.isNaN:
    false
  elif s.float in [-Inf, Inf]:
    false
  else:
    true
