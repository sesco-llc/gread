import std/options
import std/strutils
import std/random

import gread/ast

proc subtreeCrossover*[T](a, b: Ast[T]): Ast[T] =
  audit a: echo a
  audit b: echo b
  if a.len == 0:
    raise Defect.newException "xover input is empty"
  else:
    var (x, y) = (rand(a.high), rand(b.high))

    # disabling this for now; i'd rather suffer the error
    #if not a.isFunctionSymbol(x) and not b.isFunctionSymbol(y):
    # FIXME: optimize this delete/insert to a replace operation
    let c = b.subtree(y)      # the subtree at index y
    audit c: echo "chunk in xover: ", c
    result = a.replace(x, c)   # replace node(s) at x with new subtree
  audit result: echo "xover result: ", result
