import std/random

import gread/spec
import gread/ast
import gread/primitives
import gread/programs

type
  ArchKinds = enum Node, Leaf

proc randAst*[T](c: Primitives[T]; size: var int;
                 kinds = {Node, Leaf}): Ast[T] =
  ## produce a random tree of ast using `size` as a guideline
  dec size
  var kinds =
    if size <= 0: {Leaf}
            else: kinds
  case sample kinds
  of Leaf:
    result = c.initAst sample(c.terminals)
  of Node:
    let fun = sample(c.functions)
    result = c.initAst fun
    if fun.args.b > 0:
      let lo = max(fun.arity, fun.args.a)
      let hi = max(lo, fun.args.b)
      let arity = lo + rand min(size, hi-lo)
      for argument in 0..<arity:
        result = result.append(c.randAst(size), parent = 0)
  audit result: echo "random: ", result

proc randProgram*[T](c: Primitives[T]; size = 10): Program[T] =
  ## produce a random program of, roughly, the given size
  var size = size
  result = newProgram randAst(c, size, {Node})
