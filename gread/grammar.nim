import std/sequtils
import std/sets
import std/strutils
import std/macros

import pkg/adix/lptabz
import pkg/npeg

import gread/aliasmethod
import gread/ast
import gread/genotype

export ShortGenome

type
  ComponentKind* = enum ckToken, ckRule, ckTerminal
  Component* = object
    case kind*: ComponentKind
    of ckToken:
      token: int16
      text: string
    of ckRule:
      name: string
      rule: Production
    of ckTerminal:
      term: Terminal

  Production* = seq[Component]

  GrammarObj = object
    s: Component
    p: LPTab[string, Production]
    t: HashSet[Terminal]
    #n: HashSet[Function[T]]
  Grammar* = ptr GrammarObj

proc start*[T](gram: Grammar): Component = gram.s

iterator terminals*(gram: Grammar): Terminal =
  for t in gram.t.items:
    yield t

when false:
  iterator functions*[T](gram: Grammar[T]): Function[T] =
    for n in gram.n.items:
      yield n

iterator productions*(gram: Grammar; rule: string): Production =
  for key, value in gram.p.pairs:
    if key == rule:
      yield value

iterator pairs*(gram: Grammar): (string, Production) =
  for key, value in gram.p.pairs:
    yield (key, value)

proc toAst[T](prod: Production): Ast[T] =
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
                        Terminal(kind: Symbol, name: component.name))
      of ckToken:
        AstNode[T](kind: component.token)
      of ckTerminal:
        terminalNode[T](result, component.term)

proc `[]`(tab: LPTab[string, Production]; index: int): seq[Production] =
  ## find satisfying rules by index
  var i = index
  for key in tab.keys:
    if i == 0:
      for many, value in tab.pairs:
        if many == key:
          result.add value
      return result
    dec i

proc πGE*[T](gram: Grammar; geno: Genome): tuple[pc: PC; ast: Ast[T]] =
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
    terminalNode[T](result.ast, Terminal(kind: Symbol, name: "start"))

  var i: PC                                     # start at the genotype's head
  var order, codon: uint16
  while nts.len > 0:
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
    let rhs = toAst[T](rule)                    # convert rule to nodes
    doAssert rule.len == rhs.len, "not yet supported"
    result.ast = result.ast.replace(chose, rhs) # add the nodes to the ast
    for item in nts.mitems:
      if item > chose:
        item = item + (rhs.len-1)               # increment the indices
    for n, component in rule.pairs:             # add non-terminals to nts
      if component.kind == ckRule:
        nts.insert(chose + n, index)            # insert for ordering reasons

    if not canRead[uint16](geno, i, 2):         # if we're out of genome,
      if greadWrapping:                         # and wrapping is enabled,
        result.pc = i                           # record the consumed genome,
        i = default PC                          # & wrap the program counter,
      else:                                     # otherwise,
        break                                   # stop mapping

  if nts.len > 0:
    raise ShortGenome.newException "insufficient genome"

  # if we didn't set the coded portion of the genome, set it now
  if result.pc == default PC:
    result.pc = i

proc toTerminal(s: string): Terminal =
  ## turn a string into a Terminal
  try:
    let n = parseInt s
    result = Terminal(kind: Integer, intVal: n)
  except ValueError:
    try:
      let f = parseFloat s
      result = Terminal(kind: Float, floatVal: f)
    except ValueError:
      result = Terminal(kind: Symbol, name: s)

#
# we produce [void] generics here because otherwise, npeg will not
# compile due to out-of-phase sem of the peg capture clauses
#
proc bnf(s: string): seq[Component] =
  ## parse bnf to a sequence of components (production rules)
  var list: Production
  var lists: seq[Production]
  var emits: seq[Component]
  let p = peg "start":
    EOL        <- "\n" | "\r\n"
    s          <- *" "
    start      <- *rule * s * !1
    rule       <- s * "<" * >rule_name * ">" * s * "::=" * s * expression * s * EOL:
      for list in lists.items:
        emits.add Component(kind: ckRule, name: $1, rule: list)
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
          Component(kind: ckTerminal, term: toTerminal(s))
      elif s == "":
        list.add:
          Component(kind: ckTerminal, term: Terminal(kind: None))
      elif s.startsWith("<"):
        let s = s[1..^2]
        list.add Component(kind: ckRule, name: s)
      else:
        list.add Component(kind: ckToken, text: s)
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
proc bnf(gram: Grammar; s: string): seq[Component] =
  ## basically a convenience to cast the `T` type
  for rule in bnf(s).items:
    result.add rule  # lie to the compiler

func isReferential(c: Component): bool =
  ## true if the component is a pointer to a production as
  ## opposed to a materialized production definition itself
  if c.kind == ckRule:
    c.rule.len == 0
  else:
    raise ValueError.newException "nonsensical outside of production rules"

proc initGrammar*(gram: var Grammar) =
  ## initialize a grammar
  if not gram.isNil:
    dealloc gram
  gram = createShared(GrammarObj)
  if gram.isNil:
    raise ValueError.newException "unable to create grammar"
  init gram.p

proc initGrammar*[T](gram: var Grammar; syntax: string) =
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
