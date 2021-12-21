import std/json
import std/strutils

import gread/ast

const
  # demonstration of supported syntax
  glangGrammar* = """
    <start>        ::= <terminate>
    <expr>         ::= ( <value> <operator> <value> )
    <expr>         ::= <value>
    <operator>     ::= ">" | "<" | "==" | "+" | "-"
    <value>        ::= "1" | "0"
    <terminate>    ::= return <expr>
  """

type
  G* = object  ## our genetic programming language
  GKind* = enum ## some "native" glang ast node types
    Nop = "nope"
    Sym = "symbol"
    Str = "string"
    Flo = "float"
    Int = "integer"
    Bul = "bullean"
    Dad = "parent"
    Pro = "program"

    gtokReturn = "return"
    gtokIf = "if"
    gtokElse = "else"
    gtokThen = "then"
    gtokEnd = "end"
    gtokLessThan = "<"
    gtokMoreThan = ">"
    gtokEqual = "=="
    gtokLeftPar = "("
    gtokRightPar = ")"

# a convenience (which doesn't work in generic object ctors 😠)
converter toInt16(gk: GKind): int16 {.used.} = int16 gk

proc `$`*[T: G](n: AstNode[T]): string =
  ## tweaking the rendering of glang ast nodes to render our GKind
  result = $(GKind n.kind) & "." & $n.operand
  if n.flags != {}:
    result.add "/" & $n.flags

proc fun*(s: string; arity = 0; args = arity..int.high): Function[G] =
  ## shorthand for defining glang functions
  Function[G](ident: s, arity: max(arity, args.a), args: args)

proc term*(value: float): Terminal[G] =
  ## shorthand for defining glang terminals
  Terminal[G](kind: Float, floatVal: value)

proc sym*(value: string): Terminal[G] =
  ## shorthand for defining glang symbols
  Terminal[G](kind: Symbol, name: value)

# define the necessary api for gread
func isParent*[T: G](n: AstNode[T]): bool = n.kind.GKind == Dad
func isSymbol*[T: G](n: AstNode[T]): bool = n.kind.GKind == Sym
func isEmpty*[T: G](n: AstNode[T]): bool = n.kind.GKind == Nop
func isStringLit*[T: G](n: AstNode[T]): bool = n.kind.GKind == Str
func isNumberLit*[T: G](n: AstNode[T]): bool = n.kind.GKind in {Int, Flo, Bul}
func programNode*[T: G](a: var Ast[T]): AstNode[T] = AstNode[T](kind: int16 Pro)
func emptyNode*[T: G](a: var Ast[T]): AstNode[T] = AstNode[T](kind: int16 Nop)

proc terminalNode*[T: G](a: var Ast[T]; term: Terminal[T]): AstNode[T] =
  case term.kind
  of Token:
    tokenNode(a, term.token, text = term.text)
  of None:
    AstNode[T](kind: Nop.toInt16)
  of Symbol:
    AstNode[T](kind: Sym.toInt16, operand: a.strings.getOrIncl term.name)
  of String:
    AstNode[T](kind: Str.toInt16, operand: a.strings.getOrIncl term.strVal)
  of Float:
    AstNode[T](kind: Flo.toInt16,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.floatVal))
  of Integer:
    AstNode[T](kind: Int.toInt16,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.intVal))
  of Boolean:
    AstNode[T](kind: Bul.toInt16,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.boolVal))

proc composeCall*[T: G](fun: Function[T]): Ast[T] =
  result.nodes.add:
    terminalNode(result, Terminal[T](kind: Token, token: Dad.toInt16))
  result = result.append(Terminal[T](kind: Symbol, name: fun.ident), parent = 0)

proc render*[T: G](a: Ast[T]; n: AstNode[T]; index = 0): string =
  ## showing gread how to render individual glang nodes
  mixin isParent
  mixin isNumberLit
  mixin isStringLit
  mixin isEmpty
  mixin isSymbol
  if n.isParent:
    $n
  elif n.isSymbol:
    a.strings[LitId n.operand]
  elif n.isStringLit:
    escapeJson(a.strings[LitId n.operand], result)
    result
  elif n.isEmpty:
    "nil"
  elif n.kind.GKind == Flo:
    $(cast[BiggestFloat](a.numbers[LitId n.operand]))
  elif n.kind.GKind == Int:
    $a.numbers[LitId n.operand]
  elif n.kind.GKind == Bul:
    $(cast[bool](a.numbers[LitId n.operand]))
  else:
    $n.kind.GKind

proc render*(a: Ast[G]): string =
  ## render fennel ast in a form that can be compiled
  var i = 0
  var s = newSeqOfCap[int](a.len)
  template maybeAddSpace {.dirty.} =
    if result.len > 0 and result[^1] notin {'('}:
      result.add " "
  template closeParens {.dirty.} =
    while s.len > 0 and i >= s[^1]:
      result.add ")"
      discard pop s
  while i <= a.high:
    closeParens()
    maybeAddSpace()
    case a[i].kind
    of Pro:
      inc i                            # program is merely a semantic
    of Dad:
      result.add "("
      s.add i+sizeOfSubtree(a, i)      # pop when you get past index+size
      inc i
    elif a[i].isParent:
      result.add a.render(a[i], i)     # probably just a multi.symbol
      inc(i, sizeOfSubtree(a, i))      # skip the parent's subtree
    else:
      result.add a.render(a[i], i)     # rendering an individual node
      inc i
  closeParens()

proc parseToken*[T: G](s: string): GKind =
  parseEnum[GKind](s)
