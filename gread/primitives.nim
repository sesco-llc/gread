import std/hashes

#from pkg/frosty import frostyError, FreezeError, ThawError

import gread/ast

##[

this whole module is essentially deprecated by grammar

]##

type
  PrimitivesObj[T] = object
    functions*: seq[Function[T]]
    constants*: seq[Terminal[T]]
    inputs*: seq[Terminal[T]]
    outputs*: seq[Terminal[T]]
    strings: BiTable[string]
    numbers: BiTable[BiggestInt]
  Primitives*[T] = ptr PrimitivesObj[T]

proc hash*[T](c: Primitives[T]): Hash =
  hash c[]

proc newPrimitives*[T](): Primitives[T] =
  # alloc0 works, alloc does not 🙄
  when defined(gcArc) or defined(gcOrc):
    result = cast[Primitives[T]](allocShared0 sizeof(PrimitivesObj[T]))
  else:
    result = cast[Primitives[T]](alloc0 sizeof(PrimitivesObj[T]))
  result[] = PrimitivesObj[T]()

proc `functions=`*[T](c: Primitives[T];
                      a: openArray[Function[T]]) =
  c[].functions = @a

proc `inputs=`*[T](c: Primitives[T];
                   a: openArray[Terminal[T]]) =
  c[].inputs = @a

proc `outputs=`*[T](c: Primitives[T];
                    a: openArray[Terminal[T]]) =
  c[].outputs = @a

proc `constants=`*[T](c: Primitives[T];
                      a: openArray[Terminal[T]]) =
  c[].constants = @a

func terminals*[T](c: Primitives[T]): seq[Terminal[T]] =
  c[].constants & c.inputs & c.outputs

proc toLitId(x: string; c: Primitives): LitId =
  getOrIncl(c[].strings, x)

proc toLitId(x: BiggestInt; c: Primitives): LitId =
  getOrIncl(c[].numbers, x)

proc toLitId(x: BiggestUInt; c: Primitives): LitId =
  toLitId(cast[BiggestInt](x), c)

proc toLitId(x: BiggestFloat; c: Primitives): LitId =
  toLitId(cast[BiggestInt](x), c)

proc toLitId(x: bool; c: Primitives): LitId =
  toLitId(cast[BiggestInt](x), c)

proc initAst*[T](c: Primitives[T]; term: Terminal[T]): Ast[T] =
  ## support earlier api
  mixin terminalNode
  result.nodes.add result.terminalNode(term)

proc initAst*[T](c: Primitives[T]; fun: Function[T]): Ast[T] =
  ## support earlier api
  mixin composeCall
  result = composeCall fun

proc render*[T](c: Primitives[T]; a: Ast[T]): string =
  ## it's the fennel renderer for other languages 🤷
  mixin isParent
  mixin render
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
      result.add a.render(a[i])
    inc i
  closeParens()

proc toTerminal*[T](c: Primitives[T]; a: Ast; index: int): Terminal[T] =
  mixin isNumberLit
  mixin isStringLit
  mixin isEmpty
  mixin isSymbol
  template n: AstNode[T] = a[index]
  if n.isSymbol:
    Terminal[T](kind: Symbol, name: a.strings[LitId n.operand])
  elif n.isNumberLit:
    #of akFloatLit:
    #  Terminal[T](kind: Float,
    #              floatVal: cast[BiggestFloat](c[].numbers[LitId n.operand]))
    #Terminal[T](kind: Boolean,
    #            boolVal: cast[bool](c[].numbers[LitId n.operand]))
    Terminal[T](kind: Integer,
                intVal: a.numbers[LitId n.operand])
  elif n.isStringLit:
    Terminal[T](kind: String,
                strVal: a.strings[LitId n.operand])
  elif n.isEmpty:
    Terminal[T](kind: None)
  else:
    raise Defect.newException "unsupported form: " & $n

proc inputs*[T](c: Primitives[T]): var seq[Terminal[T]] = c[].inputs
proc outputs*[T](c: Primitives[T]): var seq[Terminal[T]] = c[].outputs
proc constants*[T](c: Primitives[T]): var seq[Terminal[T]] = c[].constants
proc functions*[T](c: Primitives[T]): var seq[Function[T]] = c[].functions

proc ident*[T](n: AstNode[T]; c: Primitives[T]): string =
  mixin isSymbol
  if n.isSymbol:
    c[].strings[LitId n.operand]
  else:
    raise Defect.newException "unsupported form: " & $n

when false:
  proc serialize*[S, T](output: var S; input: Primitives[T]) =
    ## used by frosty to freeze programs
    if input.isNil:
      #raise FreezeError.frostyError "unable to serialize nil primitives"
      raise
    else:
      serialize(output, 1'u8)
      serialize(output, input[])

  proc deserialize*[S, T](input: var S; output: var Primitives[T]) =
    ## used by frosty to thaw programs
    if output.isNil:
      #raise ThawError.frostyError "unable to deserialize into nil primitives"
      raise
    else:
      var v: uint8
      deserialize(input, v)
      case v
      of 1:
        deserialize(input, output[])
      else:
        #raise ThawError.frostyError "dunno how to deserialize primitives v" & $v
        raise
