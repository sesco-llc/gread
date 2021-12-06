import std/sequtils
import std/sets
import std/strutils
import std/macros

import pkg/adix/lptabz
import pkg/npeg

import gread/aliasmethod
import gread/ast
import gread/genotype

type
  ShortGenome* = object of ValueError
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
        #emptyNode[T](result)
        #
        # embed the rule name as a symbol so we can recover its name
        terminalNode[T](result,
                        Terminal[T](kind: Symbol, name: component.name))
      of ckToken:
        AstNode[T](kind: component.token)
      of ckTerminal:
        terminalNode[T](result, component.term)

proc `[]`[T](tab: LPTab[string, Production[T]]; index: int): seq[Production[T]] =
  ## find satisfying rules by index
  var i = index
  for key in tab.keys:
    if i == 0:
      for many, value in tab.pairs:
        if many == key:
          result.add value
      return result
    dec i

when false:
  proc naiveGE*[T](gram: Grammar[T]; geno: Genome): Ast[T] =
    ## map a genotype using the given grammar
    mixin programNode
    mixin toAst
    result.nodes.add programNode[T]()             # this is that head node

    var i: PC                                     # start at the genotype's head
    var codon: uint16
    while canRead[uint16](geno, i):
      geno.read(codon, i)
      let rule = gram.production(codon)           # select the correct rule
      var rhs = rule.toAst[T]()                   # convert rule to nodes
      for index, component in rule.pairs:       # resolve non-terminals
        if component.kind == ckRule:
          result.high - (rhs.high-index)
      result = result.append rhs                  # add the nodes to the result

proc πGE*[T](gram: Grammar[T]; geno: Genome): tuple[pc: PC; ast: Ast[T]] =
  ## map a genotype using the given grammar
  mixin programNode
  mixin terminalNode
  mixin toAst

  if gram.isNil:
    raise Defect.newException "unable to map genome with nil grammar"

  var nts: seq[int]                             # indices of unresolved nodes
  nts.add 0                                     # prime them with the head node
  result.ast.nodes.add:
    # we always start with the `start` production
    terminalNode[T](result.ast, Terminal[T](kind: Symbol, name: "start"))

  var i: PC                                     # start at the genotype's head
  var order, codon: uint16
  while nts.len > 0 and canRead[uint16](geno, i, 2):
    geno.read(i, order)                         # read the order codon
    geno.read(i, codon)                         # read the content codon
    let index = order.int mod nts.len           # select from non-terminals
    let chose = nts[index]                      # the LHS component to resolve
    delete(nts, index)                          # we must worry about order
    let options =
      if result.ast[chose].isSymbol:
        let name = result.ast.name(chose)       # resolve the nt name
        toSeq gram.productions(name)            # RHS production choices
      else:
        gram.p[chose] # RHS production choices
    let content = codon.int mod options.len     # choose content index
    let rule = options[content]                 # select the content production
    let rhs = rule.toAst()                      # convert rule to nodes
    doAssert rule.len == rhs.len, "not yet supported"
    result.ast = result.ast.replace(chose, rhs) # add the nodes to the ast
    for item in nts.mitems:
      if item > chose:
        item = item + (rhs.len-1)               # increment the indices
    for n, component in rule.pairs:             # add non-terminals to nts
      if component.kind == ckRule:
        nts.insert(chose + n, index)            # insert for ordering reasons

  if nts.len > 0:
    raise ShortGenome.newException "insufficient genome"

  result.pc = i  # record the program counter to indicate the mapped size

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
      result = Terminal[T](kind: Symbol, name: s)

#
# we produce [void] generics here because otherwise, npeg will not
# compile due to out-of-phase sem of the peg capture clauses
#
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

#
# here we perform a recovery of the grammar type
#
proc bnf[T](gram: Grammar[T]; s: string): seq[Component[T]] =
  ## basically a convenience to cast the `T` type
  for rule in bnf(s).items:
    result.add cast[Component[T]](rule)  # lie to the compiler

func isReferential(c: Component): bool =
  ## true if the component is a pointer to a production as
  ## opposed to a materialized production definition itself
  if c.kind == ckRule:
    c.rule.len == 0
  else:
    raise ValueError.newException "nonsensical outside of production rules"

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
  var nonterminals = 0
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
      of ckRule:
        if c.isReferential:
          inc nonterminals
        else:
          raise ValueError.newException "unexpected rule definition"
    gram.p.add(r.name, r.rule)
  echo "nonterminal references: ", nonterminals
  echo "       total terminals: ", gram.t.card
