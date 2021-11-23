import std/strutils

type
  G = object  ## our genetic programming language
  GKind = enum ## some "native" glang ast node types
    Nop = "nope"
    Sym = "symbol"
    Str = "string"
    Flo = "float"
    Int = "integer"
    Bul = "bullean"
    Dad = "parent"
    Pro = "program"
  GToken = enum
    gReturn = "return"
    gIf = "if"
    gElse = "else"
    gThen = "then"
    gEnd = "end"
    gLessThan = "<"
    gMoreThan = ">"
    gEqual = "=="
    gLeftPar = "("
    gRightPar = ")"

const
  glangGrammar = """
    <start>        ::= <terminate>
    <expr>         ::= if <expr> then <expr> else <expr> end | ( <value> <operator> <value> ) | <value>
    <operator>     ::= ">" | "<" | "==" | "+" | "-"
    <value>        ::= "1" | "0"
    <terminate>    ::= return <expr>
  """

# a convenience
converter toInt16(gk: GKind): int16 = int16 gk

proc `$`*[T: G](n: AstNode[T]): string =
  ## tweaking the rendering of glang ast nodes to render our GKind
  result = $(GKind n.kind) & "." & $n.operand
  if n.flags != {}:
    result.add "/" & $n.flags

proc fun(s: string; arity = 0; args = arity..int.high): Function[G] =
  ## shorthand for defining glang functions
  Function[G](ident: s, arity: max(arity, args.a), args: args)

proc term(value: float): Terminal[G] =
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
    AstNode[T](kind: Nop)
  of Symbol:
    AstNode[T](kind: Sym, operand: a.strings.getOrIncl term.name)
  of String:
    AstNode[T](kind: Str, operand: a.strings.getOrIncl term.strVal)
  of Float:
    AstNode[T](kind: Flo,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.floatVal))
  of Integer:
    AstNode[T](kind: Int,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.intVal))
  of Boolean:
    AstNode[T](kind: Bul,
               operand: a.numbers.getOrIncl cast[BiggestInt](term.boolVal))

proc composeCall*[T: G](fun: Function[T]): Ast[T] =
  result.nodes.add:
    terminalNode(result, Terminal[T](kind: Token, token: Dad))
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
    "«" & $n.kind & "»"

proc parseToken*[T: G](s: string): GToken =
  parseEnum[GToken](s)
