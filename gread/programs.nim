import std/hashes

import gread/ast
import gread/spec

type
  Program*[T] = ref object
    source*: int
    generation*: int
    h*: Hash
    score*: Score
    zombie*: bool
    ast*: Ast[T]

  Fitness*[T: ref] = proc(q: T; p: Program[T]): Score

func len*(p: Program): int = p.ast.len

proc `$`*[T](p: Program[T]): string =
  $p.ast

proc `<`*[T](a, b: Program[T]): bool =
  a.score < b.score

proc `==`*[T](a, b: Program[T]): bool =
  a.score == b.score

proc `<=`*[T](a, b: Program[T]): bool =
  a.score < b.score or a.score == b.score

proc hash*[T](p: Program[T]): Hash = p.h

proc newProgram*[T](a: Ast[T]): Program[T] =
  Program[T](ast: a, h: hash a, score: NaN)

proc newProgram*[T](a: Ast[T]; score: Score): Program[T] =
  Program[T](ast: a, h: hash a, score: score)

proc clone*[T](p: Program[T]): Program[T] =
  Program[T](ast: p.ast, h: p.h, score: p.score, source: p.source,
             zombie: p.zombie, generation: p.generation)
