import std/options
import std/sequtils
import std/random

import gread/spec
import gread/ast
import gread/primitives

proc mutateFunction[T](c: Primitives[T]; a: Ast[T]; i: int): Ast[T] =
  if c.isNil:
    raise Defect.newException "unable to mutate ast without primitives"
  template arity: int = countChildren(a, i) - 1
  let funs = c.functions.filterIt(arity in it.args.a .. it.args.b)
  if funs.len == 0:
    # we're here because a program we're mutating is invalid according
    # to our primitives but, probably, valid according to our target
    # language; we'll just bail since the alternative is to make the
    # situation worse...  it's possible the program will be repaired.
    return a

  # initAst produces two nodes; you want only the last one here
  a.delete(1).insert(1): @[c.initAst(sample funs).nodes[^1]]

proc pointMutation*[T](c: Primitives[T]; a: Ast[T]): Ast[T] =
  if c.isNil:
    raise Defect.newException "unable to mutate ast without primitives"
  audit result: echo "1pt mut: ", result
  let i = rand a.high
  if a[i].isParent:
    result = mutateFunction(c, a, i)
  else:
    let prior = c.toTerminal(a, i)
    # short-circuit swaps of the function call symbol
    if prior.kind == Symbol and i > 0 and a[i-1].kind == akCall:
      result = mutateFunction(c, a, i-1)
    else:
      var terms =
        case prior.kind
        of Constant:
          c.terminals.filterIt(it.kind == prior.kind and it.ck == prior.ck)
        of Symbol:
          (c.inputs & c.outputs).filterIt(it.kind == prior.kind)
      if terms.len == 0:
        echo repr(prior)
        echo repr(c.terminals)
        raise Defect.newException "unable to find suitable terminal"
      let chunk = c.initAst(sample terms)
      result = a                                      # copy the whole ast
      result.nodes[i] = chunk.nodes[0]                # swap the new node in
      if a.nodes[i].isParent:                         # if it's a parent,
        result.nodes[i].operand = a.nodes[i].operand  # fixup its length
  audit result: echo "2pt mut: ", result

proc pointPromotion*[T](c: Primitives[T]; a: Ast[T]): Ast[T] =
  if c.isNil:
    raise Defect.newException "unable to mutate ast without primitives"
  if a.len < 3 or a.countParents == 0:
    result = a
  else:
    while true:
      let i = rand 1..a.high
      # special case punt on symbols which are apparently functions
      if not a[i].isParent:
        let prior = c.toTerminal(a, i)
        if prior.kind == Symbol and a[i-1].kind == akCall:
          continue
      # okay; it's not going to present a problem...
      let p = a.parentOf(i)
      if p.isNone:
        result = a
      elif p.get == 0:
        result = a.subtree(i)
      else:
        # the parent of my parent is my parent
        let g = a.parentOf(get p)
        result = a.delete(get p)
        result = result.insert(get p, a.subtree(i)):
          # ie. parent=
          if g.isNone:
            -1
          else:
            get g
      break

proc removeOneLeaf*[T](c: Primitives[T]; a: Ast[T]; size: int): Ast[T] =
  if c.isNil:
    raise Defect.newException "unable to mutate ast without primitives"
  if a.len < 3 or a.countParents == a.len:
    result = a
  else:
    while true:
      let i = rand a.high
      if not a[i].isParent:
        let prior = c.toTerminal(a, i)
        if prior.kind != Symbol or a[i-1].kind != akCall:
          result = a.delete(i)
          break

proc appendOneLeaf*[T](c: Primitives[T]; a: Ast[T]; size: int): Ast[T] =
  if c.isNil:
    raise Defect.newException "unable to mutate ast without primitives"
  if a.len < 3:
    result = a
  else:
    block exit:
      while true:
        var i = rand a.high
        block random:
          if not a[i].isParent:
            let dad = parentOf(a, i)
            if dad.isSome:
              i = get dad
            else:
              break random  # try another index; this should be impossible
          if c.terminals.len == 0:
            raise Defect.newException "no terminals means no leaves"
          let leaf = c.initAst(sample c.terminals).nodes[^1]
          let offset = rand 0..<countChildren(a, i)  # offset some number of kids
          let index =
            if a[i].kind == akCall:
              i + 2 + offset # insert it after the function symbol
            else:
              i + 1 + offset # it's not a call?  insert it wherever...
          result = a.insert(index, @[leaf])
          break exit
