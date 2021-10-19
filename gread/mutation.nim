import std/sequtils
import std/random

import gread/spec
import gread/ast
import gread/fertilizer
import gread/primitives

proc pointMutation*[T](c: Primitives[T]; a: Ast[T]): Ast[T] =
  # FIXME: make sure we don't swap nodes with varied arity
  auditLength a
  result = a
  var point = result[rand a.len-1]
  case point.kind
  of Node:
    let x = point.node.arity
    point.node = sample: c.functions.filterIt(x in it.args.a .. it.args.b)
    point.node.arity = x
  of Leaf: point.leaf = sample c.terminals
  auditLength result

proc pointPromotion*[T](a: Ast[T]): Ast[T] =
  auditLength a
  result = a
  if a.len > 1:
    while true:
      let i = rand result.len-1
      template point: Ast[T] = result[i]
      case point.kind
      of Node:
        if point.args.len > 1:
          point = point.args[rand point.args.len-1]
          ## FIXME: switch to linear ast
          discard count result
        break
      else:
        discard

proc addOrRemoveLeaves*[T](c: Primitives[T]; a: Ast[T]; size: int): Ast[T] =
  auditLength a
  result = a
  if a.len > 1:
    while true:
      let i = rand result.len-1
      template point: Ast[T] = result[i]
      case point.kind
      of Node:
        if point.node.args.a < point.args.len and rand(1) == 0:
          discard point.args.pop
        elif point.node.args.b > point.args.len and rand(1) == 0:
          var size = size
          point.args.add randAst(c, size)
        else:
          break
        ## FIXME: switch to linear ast
        discard count result
        break
      else:
        discard
