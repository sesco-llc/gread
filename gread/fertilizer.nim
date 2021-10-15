import std/random

import gread/spec
import gread/ast

proc randAst*[T](c: Primitives[T]; size: var int;
                 kinds = {Node, Leaf}): Ast[T] =
  ## produce a random tree of ast using `size` as a guideline
  dec size
  var kinds =
    if size <= 0: {Leaf}
            else: kinds
  result =
    case sample kinds
    of Node: initAst sample(c.functions)
    of Leaf: initAst sample(c.terminals)
  if result.kind == Node and result.node.args.b > 0:
    let lo = max(result.node.arity, result.node.args.a)
    let hi = max(lo, result.node.args.b)
    result.node.arity = lo + rand min(size, hi-lo)
    for argument in 0..<result.node.arity:
      result.add:
        c.randAst(size)
    assert result.args.len >= result.node.args.a
    assert result.args.len <= result.node.args.b

proc randProgram*[T](c: Primitives[T]; size = 10): Program[T] =
  ## produce a random program of, roughly, the given size
  var size = size
  result = newProgram randAst(c, size, {Node})
