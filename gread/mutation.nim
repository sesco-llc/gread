import std/options
import std/sequtils
import std/random

import gread/spec
import gread/ast
import gread/primitives

proc mutateFunction[T](c: Primitives[T]; a: Ast[T]; i: int): Ast[T] =
  mixin composeCall
  if c.isNil:
    raise Defect.newException "unable to mutate ast without primitives"
  template arity: int = numberOfChildren(a[i]) - 1
  let funs = c.functions.filterIt(arity in it.args.a .. it.args.b)
  if funs.len == 0:
    # we're here because a program we're mutating is invalid according
    # to our primitives but, probably, valid according to our target
    # language; we'll just bail since the alternative is to make the
    # situation worse...  it's possible the program will be repaired.
    return a

  # composing a new ast with the function symbol so we can swap it in
  var ast: Ast[T]
  ast = ast.append Terminal[T](kind: Symbol, name: (sample funs).ident)
  result = a.replace(1, ast)

proc pointMutation*[T](c: Primitives[T]; a: Ast[T]): Ast[T] =
  mixin isSymbol
  mixin isParent
  audit a: echo "1pt mut: ", a
  if c.isNil:
    raise Defect.newException "unable to mutate ast without primitives"
  let i = rand a.high
  if a[i].isParent:
    result = mutateFunction(c, a, i)
  else:
    # short-circuit swaps of the function call symbol
    if a[i].isSymbol and i > 0 and a[i-1].isParent:
      result = mutateFunction(c, a, i-1)
    else:
      let prior = c.toTerminal(a, i)
      # FIXME: temporary hack for lua/fennel
      let k = if prior.kind == Integer: Float else: prior.kind
      var terms =
        case k
        of Float, Integer, String, Boolean, None, Token:
          c.terminals.filterIt(it.kind == k)
        of Symbol:
          let x = (c.inputs & c.outputs)
          if x.len < 1 or x[^1].kind != Symbol:
            echo "I/O ", repr(x)
            raise Defect.newException "unable to find suitable terminal"
          (c.inputs & c.outputs).filterIt(it.kind == k)
      if terms.len == 0:
        echo "PRIOR ", repr(prior)
        echo "TERMINALS ", repr(c.terminals)
        raise Defect.newException "unable to find suitable terminal"
      result = a.replace(i, c.initAst(sample terms))
  audit result: echo "2pt mut: ", result

proc pointPromotion*[T](c: Primitives[T]; a: Ast[T]): Ast[T] =
  mixin isSymbol
  mixin isParent
  audit a: echo "promote: ", a
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
        if prior.kind == Symbol and a[i-1].isParent:
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
  mixin isSymbol
  mixin isParent
  audit a: echo "remove: ", a
  if c.isNil:
    raise Defect.newException "unable to mutate ast without primitives"
  if a.len < 3 or a.countParents == a.len:
    result = a
  else:
    while true:
      let i = rand a.high
      if not a[i].isParent:
        if not a[i].isSymbol or not a[i-1].isParent:
          result = a.delete(i)
          break
  audit result: echo "remove result: ", result

proc appendOneLeaf*[T](c: Primitives[T]; a: Ast[T]; size: int): Ast[T] =
  mixin isSymbol
  mixin isParent
  audit a: echo "append: ", a
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
          let offset = rand 0..<numberOfChildren(a[i])  # offset some number of kids
          let index =
            if a[i].isParent:
              i + 2 + offset # insert it after the function symbol
            else:
              i + 1 + offset # it's not a call?  insert it wherever...
          result = a.insert(index, @[leaf])
          break exit
  audit result: echo "append result: ", result
