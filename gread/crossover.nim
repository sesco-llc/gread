import std/options
import std/strutils
import std/random

import gread/spec
import gread/ast

proc subtreeCrossover*[T](a, b: Ast[T]): Ast[T] =
  audit a: echo a
  audit b: echo b
  if a.len == 0:
    raise Defect.newException "xover input is empty"
  elif a.len == 1:
    result = b
  else:
    # 1 + rand ... ie. skip the root node
    var (x, y) = (rand(1..a.high), rand(b.high))

    # FIXME: optimize this delete/insert to a replace operation
    let dad = a.parentOf(x)   # parent of a subtree
    if dad.isSome:
      x = get dad
    let d = a.delete(x)       # remove the old subtree

    let mom = b.parentOf(y)   # parent of a subtree
    if mom.isSome:
      y = get mom
    let c = b.subtree(y)      # the subtree at index y

    audit d: echo "deleted in xover: ", d
    audit c: echo "chunk in xover: ", c
    result = d.insert(x, c)   # install the new subtree
  audit result: echo "xover result: ", result
