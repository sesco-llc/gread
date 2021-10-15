import std/strutils
import std/options
import std/hashes
from std/json import escapeJson

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

  AstKind* = enum Node, Leaf
  Ast*[T] = object
    len*: int
    case kind*: AstKind
    of Node:
      node*: Function[T]
      args*: seq[Ast[T]]
    of Leaf:
      leaf*: Terminal[T]

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

proc initAst*[T](a: var Ast[T]; term: Terminal[T]) =
  a = Ast[T](kind: Leaf, leaf: term, len: 1)

proc initAst*[T](term: Terminal[T]): Ast[T] =
  result.initAst term

proc initAst*[T](a: var Ast[T]; fun: Function[T]) =
  a = Ast[T](kind: Node, node: fun, len: 1)

proc initAst*[T](fun: Function[T]): Ast[T] =
  result.initAst fun

proc add*[T](a: var Ast[T]; value: Ast[T]) =
  case a.kind
  of Node:
    a.args.add value
    inc(a.len, value.len)
  else:
    raise ValueError.newException "only nodes may be added to"

proc add*[T](a: var Ast[T]; value: Terminal[T] | Function[T]) =
  a.add: initAst value

proc asAst*[T](term: Terminal[T]): Ast[T] =
  initAst term

proc asAst*[T](fun: Function[T]; args: varargs[Ast[T], asAst]): Ast[T] =
  result = initAst fun
  for a in args.items:
    result.add a

proc count*[T](a: var Ast[T]): int =
  ## FIXME: switch to linear ast
  result = 1
  if a.kind == Node:
    for n in a.args.mitems:
      if n.len == 0:
        raise IndexDefect.newException "empty length"
      else:
        inc(result, n.count)
  a.len = result

proc count*[T](a: Ast[T]): int =
  ## FIXME: switch to linear ast
  result = 1
  if a.kind == Node:
    for n in a.args.items:
      if n.len == 0:
        raise IndexDefect.newException "empty length"
      elif n.len != n.count:
        raise IndexDefect.newException "count is off: " & $n.len & " vs. " & $n.count
      else:
        inc(result, n.count)

proc hash*(a: Ast): Hash =
  var h: Hash = 0
  case a.kind
  of Node:
    h = h !& hash(a.node)
    h = h !& hash(a.args)
  of Leaf:
    h = h !& hash(a.leaf)
  result = !$h

template auditLength*[T](a: Ast[T]) =
  when not defined(release):
    if a.len == 0 or a.len != a.count:
      writeStackTrace()
      raise IndexDefect.newException:
        "length corruption: $# vs $#" % [ $a.count, $a.len ]

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

proc treeWalk*[T](a: var Ast[T]; size: int; n: var int): var Ast[T] =
  ## FIXME: switch to linear ast
  if a.len <= size:
    raise IndexDefect.newException "nonsense"
  result = a
  block done:
    if n > 0:
      dec n
      block arguments:
        for i in 0 ..< result.args.len:
          template arg: Ast[T] = result.args[i]
          auditLength result.args[i]
          if arg.len > n:
            result = arg
            case result.kind
            of Node: break arguments
            of Leaf: break done
          else:
            dec(n, arg.len)
        raise IndexDefect.newException:
          "ast.len == " & $result.len & "; " & $n & " out of range"

proc `[]`*[T](a: var Ast[T]; index: int): var Ast[T] =
  ## FIXME: switch to linear ast
  auditLength a
  var n = index
  var index = index
  result = a
  while n > 0:
    result = treeWalk(result, index, n)
    index = n

proc `[]`*[T](a: Ast[T]; index: int): Ast[T] =
  ## FIXME: switch to linear ast
  auditLength a
  var n = index
  var index = index
  result = a
  while n > 0:
    result = treeWalk(result, index, n)
    index = n
