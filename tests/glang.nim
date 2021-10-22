type
  G = object  ## our genetic programming language

  GFun = Function[G]
  GTerm = Terminal[G]
  GProg = Program[G]

proc fun(s: string; arity = 0; args = arity..int.high): GFun =
  GFun(ident: s, arity: max(arity, args.a), args: args)

proc term(value: float): GTerm =
  GTerm(kind: Constant, ck: Float, floatVal: value)

proc sym*(value: string): GTerm =
  GTerm(kind: Symbol, ident: value)

proc render[G](c: Primitives[G]; a: Ast[G]): string =
  var i = 0
  var s = newSeqOfCap[int](a.len)
  template maybeAddSpace {.dirty.} =
    if result.len > 0 and result[^1] notin {'('}:
      result.add " "
  template closeParens {.dirty.} =
    while s.len > 0 and i >= s[^1]:
      result.add ")"
      discard pop s
  while i < a.len:
    #debugEcho "..=> ", i, " ", $a[i].kind, " ", a[i].operand, " ", s
    closeParens()
    maybeAddSpace()
    if a[i].isParent:
      result.add "("
      s.add i+sizeOfSubtree(a, i)      # pop when you get past index+size
    else:
      result.add c.render(a[i])
    inc i
  closeParens()
