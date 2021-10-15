import std/strutils
import std/random

import gread/spec
import gread/ast

proc subtreeCrossover*[T](a, b: Ast[T]): Ast[T] =
  auditLength a
  auditLength b
  result = a
  # 1 + rand ... ie. skip the root node
  if a.len == 1:
    result = b
  else:
    let (x, y) = (1 + rand(a.len-2), rand(b.len-1))
    auditLength result
    system.`=`(result[x], b[y])
    ## FIXME: switch to linear ast
    discard count result[x]
    ## XXX: treewalk would be faster, right
    discard count result
  auditLength result

proc subtreeCrossover*[T](a, b: Program[T]): Ast[T] =
  auditLength a
  auditLength b
  result = subtreeCrossover(a.ast, b.ast)
  auditLength result
