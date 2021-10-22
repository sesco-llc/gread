import std/strformat
import std/monotimes
import std/options
import std/hashes
from std/json import escapeJson

import "$nim/compiler/ic/bitabs"

type
  Arity = int
  Function*[T] = object
    args*: Slice[Arity]   # XXX: types?
    arity*: Arity
    ident*: string        # XXX: switch to symbol

  SymbolKind* = enum Unknown

  ConstantKind* = enum Boolean, Integer, Float, String

  TerminalKind* = enum Constant, Symbol
  Terminal*[T] = object
    case kind*: TerminalKind
    of Constant:
      case ck*: ConstantKind
      of Float:
        floatVal*: float64
      of Boolean:
        boolVal*: bool
      of Integer:
        intVal*: int64
      of String:
        strVal*: string
    of Symbol:
      ident*: string
      sk*: SymbolKind

  NodeFlag = enum
    Compiled
    Cached
    Scored
    Optimized
    DeadCode
  NodeFlags = set[NodeFlag]

  AstOrigin = object
    generation: int32
    monotime: Monotime
    parents: (Hash, Hash)

  AstKind* = enum
    akNone
    akCall
    akIdent
    akType
    akFloatLit
    akIntLit
    akBoolLit
    akStrLit
    akNilLit

  AstNode*[T] = object
    kind*: AstKind
    operand*: int32  # length for vertices, LitId for edges
    # flags are similarly advisory only; they have no semantic bearing
    flags*: NodeFlags
    #origin: AstOrigin

  Ast*[T] = object
    # this lets us avoid playing games with a distinct seq
    nodes*: seq[AstNode[T]]

const
  akLiterals* = {akBoolLit, akIntLit, akFloatLit, akNilLit, akStrLit}
  akParents* = {akCall, akType}
  akChildren* = {akIdent} + akLiterals

template isParent*(n: AstNode): bool =
  n.kind in akParents

proc `$`*(n: AstNode): string =
  result = $n.kind & ":" & $n.operand
  if n.flags != {}:
    result.add "/" & $n.flags

proc `$`*(a: Ast): string =
  result.add "("
  for i, n in a.nodes.pairs:
    if i > 0:
      result.add ", "
    result.add $i
    result.add "."
    result.add $n
  result.add ")"

template audit*(a: Ast; logic: untyped) =
  when not defined(release):
    for i in 0..<a.nodes.len:
      let n = a.nodes[i]
      if n.kind == akNone:
        logic
        raise Defect.newException:
          "corrupted ast: node at " & $i & " is empty"
      elif n.kind in akParents and i+n.len > a.high:
        logic
        raise Defect.newException:
          "node at " & $i & " too long " & $n & " n.len " & $n.len & " a.len " & $a.len

when false:
  proc `len=`[T](n: var AstNode[T]; len: Natural) =
    case n.kind
    of akParents:
      n.operand = len.int32
    else:
      raise Defect.newException "bad kind"

proc len*[T](n: AstNode[T]): int =
  case n.kind
  of akParents:
    n.operand
  else:
    0

proc len*[T](a: Ast[T]): int =
  a.nodes.len

proc `len=`*[T](a: var Ast[T]; n: Natural) =
  setLen(a.nodes, n)

proc hash*(a: Function): Hash =
  var h: Hash = 0
  h = h !& "function".hash   # make sure functions hash uniquely
  h = h !& hash(a.arity)
  h = h !& hash(a.args)
  h = h !& hash(a.ident)
  result = !$h

proc hash*(a: Terminal): Hash =
  var h: Hash = 0
  case a.kind
  of Constant:
    h = h !& hash(a.ck)
    case a.ck
    of Float:
      h = h !& hash(a.floatVal)
    of Boolean:
      h = h !& hash(a.boolVal)
    of Integer:
      h = h !& hash(a.intVal)
    of String:
      h = h !& hash(a.strVal)
  of Symbol:
    h = h !& hash(a.ident)
    h = h !& hash(a.sk)
  h = h !& hash(a.kind)
  result = !$h

when false:
  proc initAst*[T](term: Terminal[T]): Ast[T] =
    result.initAst term

  proc initAst*[T](fun: Function[T]): Ast[T] =
    result.initAst fun

func high*(a: Ast): int {.inline.} = a.nodes.high
func low*(a: Ast): int {.inline.} = a.nodes.low

proc `[]`*[T](a: var Ast[T]; n: Natural): lent AstNode[T] {.inline.} =
  a.nodes[n]

proc `[]`*[T](a: Ast[T]; n: Natural): AstNode[T] {.inline.} =
  a.nodes[n]

proc `[]`*[T](a: Ast[T]; s: Slice[int]): Ast[T] =
  Ast[T](nodes: a.nodes[s])

proc `@`*[T](a: Ast[T]): seq[AstNode[T]] = a.nodes

iterator items[T](a: Ast[T]): AstNode[T] =
  for node in a.nodes.items:
    yield node

proc peer*[T](a: Ast[T]; index = 0): int =
  if index == a.high:
    result = -1
  elif index > a.high:
    raise IndexDefect.newException "peer index out-of-range"
  else:
    result = index + 1
    if a[index].isParent:
      inc result, a[index].operand

template numberOfChildren(n: AstNode): int =
  if isParent n:
    n.operand.int
  else:
    0

proc sizeOfSubtree*(a: Ast; index = 0): int =
  audit a: echo "sizeof subtree: ", a
  if not a[index].isParent:
    result = 1
  else:
    # count children
    var i = index  # our ptr into the ast list
    var j = 1      # the number of nodes we know remain
    while j > 0:   # we started with one, right
      if a[i].isParent:
        inc j, a[i].operand  # at least this many more remain
      inc result, 1          # count this result
      inc i                  # move to next node
      dec j                  # fewer nodes now remain

proc countAsChildren*(a: openArray[AstNode]): int =
  #audit a: echo "count as children: ", a
  var index = 0
  while index < a.len:
    if not a[index].isParent:
      inc result, 1
    else:
      # count as children
      var j = 1      # the number of nodes we know remain
      inc result, 1
      while j > 0:   # we started with one, right
        if a[index].isParent:
          inc j, a[index].operand  # at least this many more remain
        inc index                  # move to next node
        dec j                      # fewer nodes now remain
    inc index

proc subtree*(a: Ast; index: int): Ast =
  a[index ..< index+sizeOfSubtree(a, index)]

proc parentOf*[T](a: Ast[T]; index: int): Option[int] =
  var i = index - 1
  while i >= a.low:
    if a.nodes[i].isParent:
      return some i
    dec i

proc countChildren*(a: Ast; index: int): int =
  ## counts the number of direct children of the node at `index`
  numberOfChildren(a[index])

proc countParents*(a: Ast): int =
  ## counts the total number of nodes in the ast which have children
  for i in 0..<a.nodes.len:
    if a.nodes[i].kind in akParents and a.nodes[i].operand > 0:
      inc result

template copyAst[T](a, b: typed; size: int) =
  template nodeSize: int = sizeof AstNode[T]
  if size > 0:
    copyMem(addr a, unsafeAddr b, nodeSize*size)

proc delete*[T](a: Ast[T]; index: int): Ast[T] =
  audit a: echo "delete entry: ", a
  let size = a.sizeOfSubtree(index)
  if a.len - size < 0:
    debugEcho fmt"{a} index to delete: {index} and size {size}"
    raise Defect.newException:
      fmt"corrupted ast: {a.len} - {size} < 0"
  result.len = a.len - size
  # the result is the prior nodes, up to the index,
  copyAst[T](result.nodes[0], a.nodes[0], index)
  if index < a.high and index+size < a.len:
    # then add any remainder after omitting `size` nodes
    copyAst[T](result.nodes[index], a.nodes[index+size],
               (a.len - size - index))
  if index > 0:  # ie. it could have a parent we need to adjust
    let parent = a.parentOf(index)
    if parent.isSome:
      dec result.nodes[get parent].operand
  audit result:
    echo ""
    echo "delete against index ", index, " with size ", size
    echo "input: ", a
    echo "result: ", result

proc insert*[T](a: Ast[T]; index: int; values: openArray[AstNode[T]];
                parent = -1): Ast[T] =
  audit a: echo a
  if index > a.len:
    raise IndexDefect.newException:
      "bad index " & $index & " vs " & $a.len
  elif values.len == 0:
    result = a
  elif a.len == 0:
    result.nodes = @values
  else:
    # allocate new space for the insertion
    result.len = a.len + values.len
    # the result is the prior nodes, up to the index,
    copyAst[T](result.nodes[0], a.nodes[0], index)
    # then add the values we want to insert, starting at the index,
    copyAst[T](result.nodes[index], values[0], values.len)
    # then add any remainder from the original nodes
    if index < a.len:
      copyAst[T](result.nodes[index+values.len], a.nodes[index],
                 (a.len - index))
    # adjust the nearest/correct parent if possible
    block:
      var parent = parent
      if parent == -1:
        let p = a.parentOf min(index, a.high)
        if p.isSome:
          parent = get p
        else:
          break
      inc result.nodes[parent].operand, countAsChildren(values)

    audit result:
      echo "insertion index: ", index, " with parent: ", parent
      echo "insert into ast: ", a
      echo "data to insert: ", values
      echo "insert/copy result: ", result

proc insert*[T](a: Ast[T]; index: int; values: Ast[T];
                parent = -1): Ast[T] =
  audit values: echo "insert ast: ", values
  insert(a, index, values.nodes, parent = parent)

proc append*[T](a: Ast[T]; values: Ast[T]; parent = -1): Ast[T] =
  insert(a, a.len, values, parent = parent)

proc hash*(a: Ast): Hash =
  # FIXME: optimize this to just hash the chunk of memory directly
  var h: Hash = 0
  for n in a.nodes.items:
    h = h !& hash(n.kind)
    h = h !& hash(n.operand)
  result = !$h

proc `$`*(t: Terminal): string =
  case t.kind
  of Constant:
    case t.ck
    of Float:
      $t.floatVal
    of Boolean:
      $t.boolVal
    of Integer:
      $t.intVal
    of String:
      escapeJson(t.strVal, result)
      result
  of Symbol:
    t.ident

func kind*(n: AstNode): AstKind = n.kind
