type
  G = object  ## our genetic programming language

proc fun(s: string; arity = 0; args = arity..int.high): Function[G] =
  Function[G](ident: s, arity: max(arity, args.a), args: args)

proc term(value: float): Terminal[G] =
  Terminal[G](kind: Constant, ck: Float, floatVal: value)

proc `$`(a: Ast[G]): string

template joinWithSpaces[T](a: openArray[T]): string =
  map(a, `$`).join(" ")

proc `$`(a: Ast[G]): string =
  ## render fennel ast in a form that can be compiled
  case a.kind
  of Node:
    result = "("
    result.add a.node.ident
    result.add " "
    result.add joinWithSpaces(a.args)
    result.add ")"
  of Leaf:
    result = $a.leaf

proc `$`(a: Program[G]): string {.used.} = $a.ast
