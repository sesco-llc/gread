import std/hashes
import std/logging
import std/monotimes
import std/options
import std/strformat

from std/json import escapeJson

import gread/spec

import gread/bitabs
export bitabs

type
  Arity = int
  Function*[T] = object
    args*: Slice[Arity]   # XXX: types?
    arity*: Arity
    ident*: string        # XXX: switch to symbol

  TerminalKind* = enum Symbol, Boolean, Integer, Float, String, None, Token
  Terminal* = object
    case kind*: TerminalKind
    of Token:
      token*: int16
      text*: string
    of None:
      discard
    of Float:
      floatVal*: float64
    of Boolean:
      boolVal*: bool
    of Integer:
      intVal*: int64
    of String:
      strVal*: string
    of Symbol:
      name*: string

  NodeFlag* = enum
    Dead
  NodeFlags* = set[NodeFlag]

  AstNode*[T] = object
    kind*: int16
    operand*: int32  # length for parents, LitId for leaves
    # flags are similarly advisory only; they have no semantic bearing
    when false:
      flags*: NodeFlags
      origin: AstOrigin

  Ast*[T] = object
    nodes*: seq[AstNode[T]]          # avoid games with a distinct seq
    strings*: BiTable[string]        # string literals contained within
    numbers*: BiTable[BiggestInt]    # numeric literals contained within

  AstOrigin = object # not used yet...
    core: Option[int]
    population: Monotime
    generation: Generation
    monotime: Monotime
    parents: (Hash, Hash)

proc `$`*[T](n: AstNode[T]): string =
  mixin `$`
  result = $n.kind & "." & $n.operand
  when false:
    if n.flags != {}:
      result.add "/" & $n.flags

proc `$`*[T](a: Ast[T]): string =
  mixin `$`
  mixin isSymbol
  mixin isStringLit
  mixin isNumberLit
  mixin isParent
  result.add "["
  for i, n in a.nodes.pairs:
    if i > 0:
      result.add ", "
    result.add $i
    result.add ": "
    result.add $n
    if n.isSymbol:
      result.add " (" & a.stringOp(n) & ")"
    elif n.isStringLit:
      var s: string
      escapeJson(a.stringOp(n), s)
      result.add " (" & s & ")"
    elif n.isNumberLit:
      result.add " (" & $a.numberOp(n) & ")"
    elif not n.isParent and n.operand > 0:  # NOTE: assume token
      result.add " (" & a.stringOp(n) & ")"
  result.add "]"

template audit*[T](a: Ast[T]; logic: untyped) =
  mixin isParent
  when not defined(release):
    for i in 0..<a.nodes.len:
      let n = a.nodes[i]
      if n.isParent and i+n.len > a.high:
        logic
        raise Defect.newException:
          "node at " & $i & " too long " & $n & " n.len " & $n.len & " a.len " & $a.len

proc len*[T](n: AstNode[T]): int =
  mixin isParent
  if n.isParent:
    n.operand
  else:
    0

proc len*(a: Ast): int =
  a.nodes.len

proc `len=`*(a: var Ast; n: Natural) =
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
  of None:
    discard
  of Float:
    h = h !& hash(a.floatVal)
  of Boolean:
    h = h !& hash(a.boolVal)
  of Integer:
    h = h !& hash(a.intVal)
  of String:
    h = h !& hash(a.strVal)
  of Symbol:
    h = h !& hash(a.name)
  of Token:
    h = h !& hash(a.token)
    h = h !& hash(a.text)
  h = h !& hash(a.kind)
  result = !$h

proc `<`*(a, b: Terminal): bool =
  if a.kind == b.kind:
    a.hash < b.hash
  else:
    a.kind < b.kind

proc `==`*(a, b: Terminal): bool =
  a.kind == b.kind and a.hash == b.hash

func high*(a: Ast): int {.inline.} = a.nodes.high
func low*(a: Ast): int {.inline.} = a.nodes.low

proc `[]`*[T](a: var Ast[T]; n: Natural): var AstNode[T] {.inline.} =
  a.nodes[n]

proc `[]`*[T](a: Ast[T]; n: Natural): AstNode[T] {.inline.} =
  a.nodes[n]

proc `[]`*[T](a: Ast[T]; s: Slice[int]): Ast[T] =
  Ast[T](nodes: a.nodes[s], strings: a.strings, numbers: a.numbers)

proc `@`*[T](a: Ast[T]): seq[AstNode[T]] = a.nodes

proc peer*[T](a: Ast[T]; index = 0): int =
  ## the subsequent peer node's index or -1 if no such peer exists
  mixin isParent
  if index == a.high:
    result = -1
  elif index > a.high:
    raise IndexDefect.newException "peer index out-of-range"
  else:
    result = index + 1
    if a[index].isParent:
      inc result, a[index].operand

func numberOfChildren*[T](n: AstNode[T]): int =
  ## counts the number of direct children of the node
  mixin isParent
  if n.isParent:
    n.operand.int
  else:
    0

proc sizeOfSubtree*[T](a: Ast[T]; index = 0): int =
  ## the size, in nodes, of the tree at the given index
  mixin isParent
  audit a: debug "sizeof subtree: ", $a
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

proc countAsChildren*[T](a: openArray[AstNode[T]]): int =
  mixin isParent
  #audit a: debug "count as children: ", a
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
  ## the subtree at the given index, as new ast
  a[index ..< index+sizeOfSubtree(a, index)]

proc parentOf*[T](a: Ast[T]; index: int): Option[int] =
  ## the parent index of a given index, should one exist
  mixin isParent
  var i = index - 1
  while i >= a.low:
    if a.nodes[i].isParent:
      return some i
    dec i

proc countChildren*(a: Ast; index: int): int {.deprecated: "use numberOfChildren".} =
  numberOfChildren(a[index])

proc countParents*(a: Ast): int =
  ## counts the total number of nodes in the ast which have children
  mixin isParent
  for i in 0..<a.nodes.len:
    if a.nodes[i].isParent and a.nodes[i].operand > 0:
      inc result

iterator children*[T](a: Ast[T]; index: int): Ast[T] =
  ## yields each subtree of child nodes at the given parent index
  if index <= a.high:
    var kids = numberOfChildren a[index]
    var i = index + 1
    while kids > 0:
      yield subtree(a, i)
      i = peer(a, i)
      dec kids

template copyAst[T](a, b: typed; size: int) =
  ## sugar around copying ast of `size` nodes
  if size > 0:
    copyMem(addr a, unsafeAddr b, sizeof(AstNode[T])*size)

when false:
  proc learnString*(a: var Ast; s: string): LitId =
    try:
      result = a.strings[s]
    except KeyError:
      error "missing string: ", s
      raise
  proc learnNumber*(a: var Ast; n: SomeOrdinal): LitId =
    try:
      result = a.numbers[cast[BiggestInt](n.int64)]
    except KeyError:
      error "missing number: ", n.int64
      raise
else:
  template learnString*(a: var Ast; s: string): LitId =
    a.strings.getOrIncl s
  template learnNumber*(a: var Ast; n: SomeOrdinal): LitId =
    a.numbers.getOrIncl cast[BiggestInt](n.int64)
template learnNumber*(a: var Ast; n: SomeFloat): LitId =
  a.learnNumber(cast[BiggestInt](n.float64))

template stringOp*(a: Ast; op: int32 | LitId): string =
  a.strings[LitId op]
template numberOp*(a: Ast; op: int32 | LitId): BiggestInt =
  a.numbers[LitId op]
template stringOp*(a: Ast; node: AstNode): string =
  stringOp(a, node.operand)
template numberOp*(a: Ast; node: AstNode): BiggestInt =
  numberOp(a, node.operand)

when false:
  proc mergeLiterals[T](a: var Ast[T]; b: Ast[T]) =
    ## we currently just use resetLiterals subsequent to insertion
    mixin isSymbol
    mixin isStringLit
    mixin isNumberLit
    mixin isParent
    for node in b.nodes.items:
      if node.isSymbol or node.isStringLit:
        discard a.learnString b.stringOp(node)
      elif node.isNumberLit:
        discard a.learnNumber b.numberOp(node)
      elif not node.isParent and node.operand != 0:  # NOTE: assume it's a token
        discard a.learnString b.stringOp(node)

proc resetLiterals[T](a: var Ast[T]; index: int; b: Ast[T]) =
  ## fixup the operands for nodes that use string/number storage
  mixin isSymbol
  mixin isStringLit
  mixin isNumberLit
  mixin isParent
  for i in index..<(index+b.len):
    if i > a.high:     # we deleted something, i guess
      break
    template node: AstNode[T] = a.nodes[i]
    if node.isSymbol or node.isStringLit:
      node.operand = a.learnString b.stringOp(node)
    elif node.isNumberLit:
      node.operand = a.learnNumber b.numberOp(node)
    elif not node.isParent and node.operand != 0:  # NOTE: assume it's a token
      node.operand = a.learnString b.stringOp(node)

proc delete*[T](a: Ast[T]; index: int): Ast[T] =
  ## remove the node, and any of its children, at the given index
  audit a: debug "delete entry: ", a
  let size = a.sizeOfSubtree(index)
  if a.len - size < 0:
    # NOTE: unprintable ast due to mixin issues for generic `$`
    debugEcho fmt"(unprintable) index to delete: {index} and size {size}"
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

  # capture all the extant literals without regard to the deleted subtree;
  # this is simply cheaper than parsing out which values are not duplicated
  if result.len > 0:
    result.strings = a.strings
    result.numbers = a.numbers

  audit result:
    debug ""
    debug "delete against index ", index, " with size ", size
    debug "input: ", a
    debug "result: ", result

proc insert*[T](a: Ast[T]; index: int; values: Ast[T];
                parent = -1): Ast[T] =
  ## insert the `values` ast before `index` in `a`
  audit values: debug "insert ast: ", values
  audit a: debug a
  if index > a.len:
    raise IndexDefect.newException:
      "bad index " & $index & " vs " & $a.len
  elif values.len == 0:
    result = a
  elif a.len == 0:
    result = values
  else:
    # allocate new space for the insertion
    result.len = a.len + values.len

    # it's an insertion, so we'll assume the original literals
    result.strings = a.strings
    result.numbers = a.numbers

    # merge in the literals from the insertion
    when defined(mergeLiterals):
      mergeLiterals(result, values.nodes)

    # the result is the prior nodes, up to the index,
    copyAst[T](result.nodes[0], a.nodes[0], index)

    # then add the values we want to insert, starting at the index,
    copyAst[T](result.nodes[index], values.nodes[0], values.len)

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
      inc result.nodes[parent].operand, countAsChildren(values.nodes)

    # finally, reset the literal identities for the insertion
    resetLiterals(result, index, values)

    audit result:
      debug "insertion index: ", index, " with parent: ", parent
      debug "insert into ast: ", a
      debug "data to insert: ", values
      debug "insert/copy result: ", result

proc append*[T](a: Ast[T]; values: Ast[T]; parent = -1): Ast[T] =
  ## add `values` to `a`; if the `parent` index is not -1,
  ## the `values` will be children of `a[parent]`
  insert(a, a.len, values, parent = parent)

proc append*[T](a: Ast[T]; term: Terminal; parent = -1): Ast[T] =
  ## convenience to directly append a terminal to the ast
  mixin terminalNode
  var b: Ast[T]
  b.nodes.add terminalNode(b, term)
  result = a.append(b, parent = parent)

proc append*[T](a: Ast[T]; fun: Function[T]; parent = -1): Ast[T] =
  ## convenience to directly append a function to the ast
  mixin composeCall
  var b = composeCall fun
  result = a.append(b, parent = parent)

proc replace*[T](a: Ast[T]; index: int; values: Ast[T]): Ast[T] =
  ## replace the subtree in `a` at `index` with `values`
  insert(delete(a, index), index, values)

proc hash*(a: Ast): Hash =
  # FIXME: optimize this to just hash the chunk of memory directly
  var h: Hash = 0
  for n in a.nodes.items:
    h = h !& hash(n.kind)
    h = h !& hash(n.operand)
  h = h !& hash(a.numbers)
  h = h !& hash(a.strings)
  result = !$h

proc `$`*(t: Terminal): string =
  case t.kind
  of None:
    "nil"
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
    t.name
  of Token:
    fmt"«{t.token}:{t.text}»"

proc tokenNode*[T](a: var Ast[T]; token: int16; text = ""): AstNode[T] =
  ## produce a token node; stores the token's textual form if it's provided
  let op =
    if text == "":
      LitId 0
    else:
      a.learnString(text)
  AstNode[T](kind: token, operand: int32 op)

proc name*[T](a: Ast[T]; index: int): string =
  ## convenience to fetch the name of a symbol at the given index
  mixin isSymbol
  if a[index].isSymbol:
    result = stringOp(a, a[index])
  else:
    raise Defect.newException "node is not a symbol"
