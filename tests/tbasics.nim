import std/json
import std/options
#import std/strutils
import std/sequtils
import std/math

import pkg/balls

import gread
import gread/grammar

include glang

var prims = newPrimitives[G]()
prims.functions = @[fun"+"]

proc asAst*[T](term: Terminal[T]; c: Primitives[T]): Ast[T] =
  ## convenience
  c.initAst term

proc asAst*[T](fun: Function[T]; c: Primitives[T];
               args: varargs[Ast[T], asAst]): Ast[T] =
  ## convenience
  result = c.initAst fun
  for a in args.items:
    result = result.insert(result.len, a)

when false:
  var j = prims.initAst fun"+"
  j = j.append prims.initAst(term 2.0)
  j = j.append prims.initAst(term 3.0)

suite "basic machinery":
  block:
    ## ast node
    var a = prims.initAst fun"+"
    check a.len == 2
    check a[0].kind == Dad
    check a[0].isParent
    check a.sizeOfSubtree(0) == 2
    checkpoint prims.render(a)
    check prims.render(a) == "(+)"
    check countParents(a) == 1
    check numberOfChildren(a[0]) == 1
    check numberOfChildren(a[1]) == 0
    check a.peer(0) == 2
    check a.peer(1) == -1
    a = delete(a, a.high)
    check a.len == 1
    check prims.render(a) == "()"

  block:
    ## ast node plus leaf
    var a = prims.initAst fun"+"
    a = a.append prims.initAst(term 2.0)
    check a.len == 3
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 2.0)"
    check countParents(a) == 1
    check numberOfChildren(a[0]) == 2
    check numberOfChildren(a[1]) == 0
    check numberOfChildren(a[2]) == 0
    # this ast is broken... it shouldn't render, right?
    a = delete(a, 1)
    check a.len == 2
    checkpoint prims.render(a)
    check prims.render(a) == "(2.0)"

  block:
    ## ast node plus leaves
    var a = prims.initAst fun"+"
    a = a.append prims.initAst(term 2.0)
    a = a.append prims.initAst(term 3.0)
    check a.len == 4
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 2.0 3.0)"
    check a.render(a[1]) == "+"
    check a.render(a[2]) == "2.0"
    check a.render(a[3]) == "3.0"
    check a[2].kind == Flo
    check countParents(a) == 1
    check numberOfChildren(a[0]) == 3
    a = delete(a, 2)
    check a.len == 3
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 3.0)"

  block:
    ## tree with branches (shape A)
    var a = prims.initAst fun"+"
    a = a.append prims.initAst(term 2.0)
    let dad = a.parentOf a.high
    let (c, d) = (prims.initAst(term 6.0), prims.initAst(term 1.0))
    var b = asAst(fun"-", prims)
    b = b.append c
    b = b.append d
    a = a.append b
    a = a.append(prims.initAst(term 3.0), parent = get dad)
    check a.len == 8
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 2.0 (- 6.0 1.0) 3.0)"
    check countParents(a) == 2
    check numberOfChildren(a[0]) == 4
    check numberOfChildren(a[3]) == 3
    a = delete(a, 2)
    check a.len == 7
    checkpoint prims.render(a)
    check prims.render(a) == "(+ (- 6.0 1.0) 3.0)"

  block:
    ## tree with branches (shape B)
    var a = prims.initAst fun"+"
    let dad = a.parentOf a.high
    let (c, d) = (prims.initAst(term 6.0), prims.initAst(term 1.0))
    var b = asAst(fun"-", prims)
    b = b.append c
    b = b.append d
    a = a.append b
    a = a.append(prims.initAst(term 3.0), parent = get dad)
    check a.len == 7
    checkpoint prims.render(a)
    check prims.render(a) == "(+ (- 6.0 1.0) 3.0)"
    check countParents(a) == 2
    check numberOfChildren(a[0]) == 3
    check numberOfChildren(a[2]) == 3
    check sizeOfSubtree(a, 2) == 4
    check a.parentOf(2).get == 0
    a = delete(a, 2)
    check a.len == 3
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 3.0)"

  block:
    ## tree with branches (shape C)
    var a = prims.initAst fun"+"
    a = a.insert(a.len, prims.initAst(term 3.0))
    let (c, d) = (prims.initAst(term 6.0), prims.initAst(term 1.0))
    var b = asAst(fun"-", prims)
    b = b.insert(b.len, c)
    b = b.insert(b.len, d)
    a = a.insert(a.len, b)
    check a.len == 7
    checkpoint prims.render(a)
    check prims.render(a) == "(+ 3.0 (- 6.0 1.0))"
    check countParents(a) == 2
    check numberOfChildren(a[0]) == 3
    check numberOfChildren(a[3]) == 3

  block:
    ## supporting math
    let a = [1.0, 2.0, 3.0, 4.0, 5.0]
    let b = [2.0, 3.0, 4.0, 5.0, 6.0]
    let r2 = sqrt 2.0
    check "is stats.nim really worse?":
      variance(a) == 2.0
      variance(b) == 2.0
      covariance(a, b) == 2.0
      stddev(a) == r2
      correlation(a, b) == (2.0 / (r2*r2))

suite "trees":
  var prims = newPrimitives[G]()
  var trees: seq[Program[G]]
  block:
    ## tree tests
    prims.constants = @[term 1, term 2, term 3, sym"a", sym"b", sym"c"]
    prims.functions = @[fun"+", fun"-", fun"/", fun"*"]
    while trees.len < 10_000:
      trees.add:
        randProgram(prims, size=10)  #
    let sizes = mapIt(trees, it.len)
    checkpoint "average size: ", avg(sizes)

  block:
    ## dig them spinners
    for i, p in trees.pairs:
      checkpoint render(prims, p.ast)
      if i == 10:
        break

suite "data":
  block:
    ## testing data operations
    let stuff =
      initSymbolSet[G, JsonNode]:
        {
          "a": newJInt 4,
          "b": newJString "hello",
          "c": newJFloat 3.5,
        }
    check stuff.len == 3
    var count = stuff.len
    for name, point in stuff.pairs:
      check name in ["a", "b", "c"]
      case point.value.kind
      of JInt:      check point.getInt == 4
      of JString:   check point.getStr == "hello"
      of JFloat:    check point.getFloat == 3.5
      else: fail"unexpected json kind"
      dec count
    check count == 0, "missing some items"

    for point in stuff.items:
      case point.value.kind
      of JInt:      check $point == "a=4"
      of JString:   check $point == "b=\"hello\""
      of JFloat:    check $point == "c=3.5"
      else: fail"unexpected json kind"

suite "grammar":
  block:
    ## parse a grammar
    const example = """
     <start>          ::= <rule> | <rule> <start>
     <rule>           ::= <opt-whitespace> "<" <rule-name> ">" <opt-whitespace> "::=" <opt-whitespace> <expression> <line-end>
     <opt-whitespace> ::= " " <opt-whitespace> | ""
     <expression>     ::= <list> | <list> <opt-whitespace> "|" <opt-whitespace> <expression>
     <line-end>       ::= <opt-whitespace> <EOL> | <line-end> <line-end>
     <list>           ::= <term> | <term> <opt-whitespace> <list>
     <term>           ::= <literal> | "<" <rule-name> ">"
     <literal>        ::= '"' <text1> '"' | "'" <text2> "'"
     <text1>          ::= "" | <character1> <text1>
     <text2>          ::= '' | <character2> <text2>
     <character>      ::= <letter> | <digit> | <symbol>
     <letter>         ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
     <digit>          ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
     <symbol>         ::=  "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"
     <character1>     ::= <character> | "'"
     <character2>     ::= <character> | '"'
     <rule-name>      ::= <letter> | <rule-name> <rule-char>
     <rule-char>      ::= <letter> | <digit> | "-"
    """

    var gram: Grammar[G]
    initGrammar gram
    for s in gram.bnf(example):
      check s.kind == ckRule
      check s.name != ""
      check s.rule.len > 0
      checkpoint s.name, " ", s.rule.len
      #echo repr(s)
