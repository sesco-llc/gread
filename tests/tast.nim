import std/json
import std/sets
import std/options
import std/sequtils

import pkg/balls

import gread/ast
import gread/programs
import gread/primitives

include ./glang

var prims = newPrimitives[G]()
prims.functions = @[fun"+"]

proc asAst*[T](term: Terminal[T]; c: Primitives[T]): Ast[T] =
  ## convenience
  c.initAst term

proc asAst*[T](fun: Function[T]; c: Primitives[T];
               args: varargs[Ast[T], asAst]): Ast[T] =
  ## convenience
  result = c.initAst fun
  for a in args.items:
    result = result.insert(result.len, a)

when false:
  var j = prims.initAst fun"+"
  j = j.append prims.initAst(term 2.0)
  j = j.append prims.initAst(term 3.0)

suite "basic machinery":
  block:
    ## ast node
    var a = prims.initAst fun"+"
    check a.len == 2
    check a[0].kind == Dad
    check a[0].isParent
    check a.sizeOfSubtree(0) == 2
    checkpoint prims.render(a)
    check prims.render(a) == "(+)"
    check countParents(a) == 1
    check numberOfChildren(a[0]) == 1
    check numberOfChildren(a[1]) == 0
    check a.peer(0) == 2
    check a.peer(1) == -1
    a = delete(a, a.high)
    check a.len == 1
    check prims.render(a) == "()"

  block:
    ## ast node plus leaf
    var a = prims.initAst fun"+"
    a = a.append prims.initAst(term 2.0)
    check a.len == 3
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 2.0)"
    check countParents(a) == 1
    check numberOfChildren(a[0]) == 2
    check numberOfChildren(a[1]) == 0
    check numberOfChildren(a[2]) == 0
    # this ast is broken... it shouldn't render, right?
    a = delete(a, 1)
    check a.len == 2
    checkpoint prims.render(a)
    check prims.render(a) == "(2.0)"

  block:
    ## ast node plus leaves
    var a = prims.initAst fun"+"
    a = a.append prims.initAst(term 2.0)
    a = a.append prims.initAst(term 3.0)
    check a.len == 4
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 2.0 3.0)"
    check a.render(a[1]) == "+"
    check a.render(a[2]) == "2.0"
    check a.render(a[3]) == "3.0"
    check a[2].kind == Flo
    check countParents(a) == 1
    check numberOfChildren(a[0]) == 3
    a = delete(a, 2)
    check a.len == 3
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 3.0)"

  block:
    ## tree with branches (shape A)
    var a = prims.initAst fun"+"
    a = a.append prims.initAst(term 2.0)
    let dad = a.parentOf a.high
    let (c, d) = (prims.initAst(term 6.0), prims.initAst(term 1.0))
    var b = asAst(fun"-", prims)
    b = b.append c
    b = b.append d
    a = a.append b
    a = a.append(prims.initAst(term 3.0), parent = get dad)
    check a.len == 8
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 2.0 (- 6.0 1.0) 3.0)"
    check countParents(a) == 2
    check numberOfChildren(a[0]) == 4
    check numberOfChildren(a[3]) == 3
    a = delete(a, 2)
    check a.len == 7
    checkpoint prims.render(a)
    check prims.render(a) == "(+ (- 6.0 1.0) 3.0)"

  block:
    ## tree with branches (shape B)
    var a = prims.initAst fun"+"
    let dad = a.parentOf a.high
    let (c, d) = (prims.initAst(term 6.0), prims.initAst(term 1.0))
    var b = asAst(fun"-", prims)
    b = b.append c
    b = b.append d
    a = a.append b
    a = a.append(prims.initAst(term 3.0), parent = get dad)
    check a.len == 7
    checkpoint prims.render(a)
    check prims.render(a) == "(+ (- 6.0 1.0) 3.0)"
    check countParents(a) == 2
    check numberOfChildren(a[0]) == 3
    check numberOfChildren(a[2]) == 3
    check sizeOfSubtree(a, 2) == 4
    check a.parentOf(2).get == 0
    a = delete(a, 2)
    check a.len == 3
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 3.0)"

  block:
    ## tree with branches (shape C)
    var a = prims.initAst fun"+"
    a = a.insert(a.len, prims.initAst(term 3.0))
    let (c, d) = (prims.initAst(term 6.0), prims.initAst(term 1.0))
    var b = asAst(fun"-", prims)
    b = b.insert(b.len, c)
    b = b.insert(b.len, d)
    a = a.insert(a.len, b)
    check a.len == 7
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 3.0 (- 6.0 1.0))"
    check countParents(a) == 2
    check numberOfChildren(a[0]) == 3
    check numberOfChildren(a[3]) == 3
