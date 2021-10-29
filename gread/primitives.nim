from std/json import escapeJson
import gread/ast

import "$nim/compiler/ic/bitabs"

type
  Primitives*[T] = ref object
    functions*: seq[Function[T]]
    constants*: seq[Terminal[T]]
    inputs*: seq[Terminal[T]]
    outputs*: seq[Terminal[T]]
    strings: BiTable[string]
    numbers: BiTable[BiggestInt]

proc newPrimitives*[T](): Primitives[T] =
  new Primitives[T]

func terminals*[T](c: Primitives[T]): seq[Terminal[T]] =
  c.constants & c.inputs & c.outputs

proc toLitId(x: string; c: Primitives): LitId =
  getOrIncl(c.strings, x)

proc toLitId(x: BiggestInt; c: Primitives): LitId =
  getOrIncl(c.numbers, x)

proc toLitId(x: BiggestUInt; c: Primitives): LitId =
  toLitId(cast[BiggestInt](x), c)

proc toLitId(x: BiggestFloat; c: Primitives): LitId =
  toLitId(cast[BiggestInt](x), c)

proc toLitId(x: bool; c: Primitives): LitId =
  toLitId(cast[BiggestInt](x), c)

proc initAst*[T](c: Primitives[T]; term: Terminal[T]): Ast[T] =
  result.nodes.add:
    case term.kind
    of Constant:
      case term.ck
      of Float:
        AstNode[T](kind: akFloatLit, operand: term.floatVal.toLitId(c).int32)
      of Boolean:
        AstNode[T](kind: akBoolLit, operand: term.boolVal.toLitId(c).int32)
      of Integer:
        AstNode[T](kind: akIntLit, operand: term.intVal.toLitId(c).int32)
      of String:
        AstNode[T](kind: akStrLit, operand: term.strVal.toLitId(c).int32)
    of Symbol:
      AstNode[T](kind: akIdent, operand: term.ident.toLitId(c).int32)

proc initAst*[T](c: Primitives[T]; fun: Function[T]): Ast[T] =
  result.nodes.add:
    @[
      AstNode[T](kind: akCall, operand: 1),
      AstNode[T](kind: akIdent, operand: fun.ident.toLitId(c).int32)
    ]

proc asAst*[T](term: Terminal[T]; c: Primitives[T]): Ast[T] =
  c.initAst term

proc asAst*[T](fun: Function[T]; c: Primitives[T];
               args: varargs[Ast[T], asAst]): Ast[T] =
  result = c.initAst fun
  for a in args.items:
    result = result.insert(result.len, a)

proc render*[T](c: Primitives[T]; n: AstNode[T]): string =
  case n.kind
  of akParents:
    "→"
  of akIdent:
    c.strings[LitId n.operand]
  of akStrLit:
    escapeJson(c.strings[LitId n.operand], result)
    result
  of akNilLit:
    "nil"
  of akBoolLit:
    $cast[bool](c.numbers[LitId n.operand])
  of akIntLit:
    $cast[BiggestInt](c.numbers[LitId n.operand])
  of akFloatLit:
    $cast[BiggestFloat](c.numbers[LitId n.operand])
  else:
    "«" & $n.kind & "»"

proc render*[T](c: Primitives[T]; a: Ast[T]): string =
  ## it's the fennel renderer for other languages 🤷
  if c.isNil:
    raise Defect.newException "unable to render without primitives"
  audit a: echo "render: ", repr(a)
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
    closeParens()
    maybeAddSpace()
    if a[i].isParent:
      result.add "("
      s.add i+sizeOfSubtree(a, i)      # pop when you get past index+size
    else:
      result.add c.render(a[i])
    inc i
  closeParens()

proc toTerminal*[T](c: Primitives[T]; a: Ast; index: int): Terminal[T] =
  template n: AstNode[T] = a[index]
  case n.kind
  of akIdent:
    Terminal[T](kind: Symbol, ident: c.strings[LitId n.operand])
  of akFloatLit:
    Terminal[T](kind: Constant, ck: Float,
                floatVal: cast[BiggestFloat](c.numbers[LitId n.operand]))
  of akIntLit:
    Terminal[T](kind: Constant, ck: Integer,
                intVal: c.numbers[LitId n.operand])
  of akStrLit:
    Terminal[T](kind: Constant, ck: String,
                strVal: c.strings[LitId n.operand])
  of akBoolLit:
    Terminal[T](kind: Constant, ck: Boolean,
                boolVal: cast[bool](c.numbers[LitId n.operand]))
  else:
    raise Defect.newException "unsupported form: " & $n

proc isFunctionSymbol*(a: Ast; index: int): bool =
  if index > 0 and index < a.high:
    if not a[index].isParent:
      result = a[index].kind == akIdent and a[index-1].kind == akCall
