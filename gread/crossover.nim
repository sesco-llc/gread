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
  else:
    while true:
      var (x, y) = (rand(a.high), rand(b.high))

      # disabling this for now; i'd rather suffer the error
      #if not a.isFunctionSymbol(x) and not b.isFunctionSymbol(y):
      when true:
        # FIXME: optimize this delete/insert to a replace operation
        let d = a.delete(x)       # remove the old subtree
        let c = b.subtree(y)      # the subtree at index y

        audit d: echo "deleted in xover: ", d
        audit c: echo "chunk in xover: ", c
        result = d.insert(x, c)   # install the new subtree
        break
  audit result: echo "xover result: ", result
