import std/sets
import std/strutils
import std/macros

import pkg/adix/lptabz
import pkg/npeg

import gread/aliasmethod
import gread/ast

type
  ComponentKind* = enum ckToken, ckRule, ckTerminal
  Component*[T] = object
    case kind*: ComponentKind
    of ckToken:
      token: int16
      text: string
    of ckRule:
      name: string
      rule: Production[T]
    of ckTerminal:
      term: Terminal[T]

  Production*[T] = seq[Component[T]]

  GrammarObj[T] = object
    s: Component[T]
    p: LPTab[string, Production[T]]
    t: HashSet[Terminal[T]]
    n: HashSet[Function[T]]
  Grammar*[T] = ptr GrammarObj[T]

  Genotype* = seq[int]

proc start*[T](gram: Grammar[T]): Component[T] = gram.s

iterator terminals*[T](gram: Grammar[T]): Terminal[T] =
  for t in gram.t.items:
    yield t

iterator functions*[T](gram: Grammar[T]): Function[T] =
  for n in gram.n.items:
    yield n

iterator productions*[T](gram: Grammar[T]; rule: string): Production[T] =
  for key, value in gram.p.pairs:
    if key == rule:
      yield value

iterator pairs*[T](gram: Grammar[T]): (string, Production[T]) =
  for key, value in gram.p.pairs:
    yield (key, value)

proc toAst[T](prod: Production[T]): Ast[T] =
  ## map a production to matching ast
  mixin emptyNode
  mixin terminalNode
  result.nodes = newSeqOfCap[AstNode[T]](prod.len)
  for component in prod.items:
    result.nodes.add:
      case component.kind
      of ckRule:
        emptyNode[T]()
      of ckToken:
        AstNode[T](kind: component.token)
      of ckTerminal:
        terminalNode[T](component.term)

proc πGE*[T](gram: Grammar[T]; geno: Genotype): Ast[T] =
  ## map a genotype using the given grammar
  mixin programNode
  var nts: seq[int]                             # indices of unresolved nodes
  nts.add 0                                     # prime them with the head node
  result.nodes.add programNode[T]()             # this is that head node

  var i = 0                                     # start at the genotype's head
  while i+2 < geno.high and nts.len > 0:
    let order = geno[i]; inc i                  # order coden
    let codon = geno[i]; inc i                  # content coden
    let index = order mod nts.len               # select from non-terminals list
    let chose = nts[index]                      # the LHS component to resolve
    let options = gram.p[chose]                 # RHS productions to choose from
    let rule = options[codon mod options[].len] # select the correct rule
    let rhs = rule.toAst[T]()                   # convert rule to nodes
    result = result.append rhs                  # add the nodes to the result
    for index, component in rule.pairs:         # add non-terminals to nts
      if component.kind == ckRule:
        nts.add:
          result.high - (rhs.high-index)

proc toTerminal[T](s: string): Terminal[T] =
  ## turn a string into a Terminal
  try:
    let n = parseInt s
    result = Terminal[T](kind: Integer, intVal: n)
  except ValueError:
    try:
      let f = parseFloat s
      result = Terminal[T](kind: Float, floatVal: f)
    except ValueError:
      result = Terminal[T](kind: String, strVal: s)

proc bnf(s: string): seq[Component[void]] =
  ## parse bnf to a sequence of components (production rules)
  var list: Production[void]
  var lists: seq[Production[void]]
  var emits: seq[Component[void]]
  let p = peg "start":
    EOL        <- "\n" | "\r\n"
    s          <- *" "
    start      <- *rule * s * !1
    rule       <- s * "<" * >rule_name * ">" * s * "::=" * s * expression * s * EOL:
      for list in lists.items:
        emits.add Component[void](kind: ckRule, name: $1, rule: list)
      setLen(lists, 0)
    expression <- list * s * *("|" * s * list)
    list       <- *(term * s):
      lists.add list
      setLen(list, 0)
    term       <- >literal | >token | >("<" * rule_name * ">"):
      let s = $1
      if s.startsWith("\"") or s.startsWith("\'"):
        let s = s[1..^2]
        list.add:
          Component[void](kind: ckTerminal, term: toTerminal[void](s))
      elif s == "":
        list.add:
          Component[void](kind: ckTerminal,
                          term: Terminal[void](kind: None))
      elif s.startsWith("<"):
        let s = s[1..^2]
        list.add Component[void](kind: ckRule, name: s)
      else:
        list.add Component[void](kind: ckToken, text: s)
    token      <- (symbolNoS | Alpha) * *(symbolNoS | Alpha | Digit)
    literal    <- '"' * text1 * '"' | "'" * text2 * "'"
    text1      <- *("\"\"" | character1)
    text2      <- *("''" | character2)
    character  <- Alpha | Digit | symbol
    symbolNoS  <- {'!','#','$','%','&','(',')','*','+',',','-','.','/',':',';','>','=','<','?','@','[','\\',']','^','_','`','{','}','~'}
    symbol     <- {'|',' ','!','#','$','%','&','(',')','*','+',',','-','.','/',':',';','>','=','<','?','@','[','\\',']','^','_','`','{','}','~'}
    character1 <- character | "'"
    character2 <- character | '"'
    rule_name  <- Alpha * *rule_char
    rule_char  <- Alpha | Digit | "-"

  let r = p.match(s)
  if r.ok:
    result = emits
  else:
    raise ValueError.newException:
      "error parsing grammar at `$#`" %
        s[(r.matchLen-1)..min(s.high, r.matchMax+30)]

proc bnf[T](gram: Grammar[T]; s: string): seq[Component[T]] =
  ## basically a convenience to cast the `T` type
  for rule in bnf(s).items:
    result.add cast[Component[T]](rule)  # lie to the compiler

proc initGrammar*[T](gram: var Grammar[T]) =
  ## initialize a grammar
  if not gram.isNil:
    dealloc gram
  gram = createShared(GrammarObj[T])
  if gram.isNil:
    raise ValueError.newException "unable to create grammar"
  init gram.p

proc initGrammar*[T](gram: var Grammar[T]; syntax: string) =
  ## initialize a grammar with the provided bnf syntax specification
  mixin parseToken
  initGrammar gram
  var rules = gram.bnf(syntax)
  for r in rules.mitems:
    for c in r.rule.mitems:
      case c.kind
      of ckToken:
        c.token = int16 parseToken[T](c.text)
      of ckTerminal:
        if c.term.kind == Token:
          c.term.token = int16 parseToken[T](c.term.text)
        else:
          gram.t.incl c.term
      else:
        discard
    gram.p.add(r.name, r.rule)
