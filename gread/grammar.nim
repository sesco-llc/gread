import std/hashes
import std/logging
import std/macros
import std/md5
import std/sequtils
import std/sets
import std/strformat
import std/strutils

import pkg/npeg
import pkg/insideout/atomic/refs
export refs

import gread/aliasmethod
import gread/ast
import gread/audit
import gread/genotype
import gread/programs
import gread/spec

export ShortGenome

import std/locks

type
  HeatMap = GreadTable[string, int]
  OrderedProductions = GreadTable[string, seq[Production]]
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
  WeightedProductions = GreadOrderedTable[string, AliasMethod[Production]]

  GrammarObj = object
    s: Component
    p: OrderedProductions
    #w: WeightedProductions
    t: HashSet[Terminal]
    h: MD5Digest
    strings: BiTable[string]
    numbers: BiTable[BiggestInt]
    when defined(greadGrammarHeatMap):
      m: HeatMap
      mlock: Lock
  Grammar* = AtomicRef[GrammarObj]

template withHeatMap(gram: Grammar; body: untyped): untyped =
  when compiles(gram[].mlock):
    withLock gram[].mlock:
      body

proc cmp(a, b: Component): int =
  result = system.cmp(a.kind, b.kind)
  if result == 0:
    case a.kind
    of ckTerminal:
      result = system.cmp(a.term, b.term)
    of ckToken:
      result = system.cmp(a.token, b.token)
      if result == 0:
        result = system.cmp(a.text, b.text)
    of ckRule:
      result = system.cmp(a.name, b.name)

proc `<`(a, b: Component): bool =
  cmp(a, b) == -1

proc `==`(a, b: Component): bool =
  cmp(a, b) == 0

proc hash*(gram: Grammar): Hash =
  hash gram[].h

proc start*[T](gram: Grammar): Component =
  gram[].s

iterator terminals*(gram: Grammar): Terminal =
  for t in gram[].t.items:
    yield t

proc productions*(gram: Grammar; rule: string): seq[Production] =
  for key, value in gram[].p.pairs:
    if key == rule:
      result.add value

proc toAst[T](gram: Grammar; prod: Production): Ast[T] =
  ## map a production to matching ast
  mixin emptyNode
  mixin terminalNode
  result.nodes = newSeqOfCap[AstNode[T]](prod.len)
  result.strings = gram[].strings
  result.numbers = gram[].numbers
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

proc πGE*[T](gram: Grammar; geno: Genome): tuple[pc: PC; ast: Ast[T]] =
  ## map a genotype using the given grammar
  mixin programNode
  mixin terminalNode
  mixin toAst

  if gram.isNil:
    raise Defect.newException "unable to map genome with nil grammar"

  if geno.len == 0:
    raise Defect.newException "supplied genome is empty"

  var nts: seq[int]                             # indices of unresolved nodes
  nts.add 0                                     # prime them with the head node
  result.ast.nodes.add:
    # we always start with the `start` production
    terminalNode[T](result.ast, Terminal(kind: Symbol, name: "start"))

  var i: PC                                     # start at the genotype's head
  var order, codon: uint16
  while nts.len > 0:
    if not canRead[uint16](geno, i, 2):           # if we're out of genome,
      raise ShortGenome.newException "insufficient genome"

    geno.read(i, order)                         # read the order codon
    geno.read(i, codon)                         # read the content codon

    # chose represents a non-terminal index in the ast which
    # we have not yet resolved; we'll replace it now...
    let index = order.int mod nts.len           # select from non-terminals
    let chose = nts[index]                      # the LHS component to resolve
    delete(nts, index)                          # we must worry about order
    let options =
      # if it's a symbol in the ast, treat it as a non-terminal name
      if result.ast[chose].isSymbol:
        let name = result.ast.name(chose)       # resolve the nt name
        withHeatMap gram:
          inc gram.m[name]
        gram[].p[name]                          # RHS production choices
      else:
        raise Defect.newException "bug.  bad indices in nts queue"

    # guard a grammar defect
    if options.len == 0:
      raise Defect.newException:
        "bug.  no productions for " & result.ast.name(chose)

    # content points to the production index from the `chose`n options
    let content = codon mod options.len.uint16  # choose content index
    let rule = options[int content]             # select the content production
    let rhs = toAst[T](gram, rule)              # convert rule to nodes
    assert rule.len == rhs.len, "not yet supported"

    # we can now perform the substitution in the ast at the `chose`n index
    result.ast = result.ast.replace(chose, rhs) # add the nodes to the ast

    # adjust the remaining non-terminal indices so they refer to
    # the same nodes in the ast
    for item in nts.mitems:
      if item > chose:
        item = item + rhs.high                  # increment the indices

    # add any new non-terminals to the queue; we exploit rule.len == rhs.len
    # so this is perhaps more subtle than it appears...
    for n, component in rule.pairs:             # add non-terminals to nts
      if component.kind == ckRule:
        nts.insert(chose + n, index)            # insert for ordering reasons

  # set the coded portion of the genome
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

proc bnf(s: string): seq[Component] =
  ## parse bnf to a sequence of components (production rules)
  var list: Production
  var prods: seq[Production]
  var emits: seq[Component]
  var s = s.strip() & "\n"  # normalize input
  let p = peg "start":
    EOL        <- "\n" | "\r\n"
    s          <- *" "
    start      <- *rule * s * !1
    rule       <- s * "<" * >rule_name * ">" * s * "::=" * s * expression * s * EOL:
      for list in prods.items:
        emits.add Component(kind: ckRule, name: $1, rule: list)
      setLen(prods, 0)
    expression <- list * s * *("|" * s * list)
    list       <- *(term * s):
      prods.add list
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

func isReferential(c: Component): bool =
  ## true if the component is a pointer to a production as
  ## opposed to a materialized production definition itself
  if c.kind == ckRule:
    c.rule.len == 0
  else:
    raise ValueError.newException "nonsensical outside of production rules"

proc isInitialized*(gram: Grammar): bool =
  ## true if the grammar has been initialized 🙄
  not gram.isNil

proc initGrammar*(gram: var Grammar) =
  ## initialize a grammar
  new gram
  if gram.isNil:
    raise ValueError.newException "unable to create grammar"
  init gram[].t
  initGreadTable gram[].p
  #initGreadOrderedTable gram.w
  withHeatMap gram:
    initGreadTable gram[].m

proc `$`*(gram: Grammar): string =
  withHeatMap gram:
    for key, value in gram.m.pairs:
      result &= "$# -> $#\n" % [ key, $value ]

proc initGrammar*(gram: var Grammar; parseToken: proc(s: string): int16;
                  syntax: string) =
  ## initialize a grammar with the provided bnf syntax specification
  initGrammar gram
  gram[].h = toMD5(syntax)
  var rules = bnf(syntax)
  var nonterminals = 0
  template learnString(s: string) =
    discard gram[].strings.getOrIncl s
  template learnNumber(n: SomeInteger) =
    discard gram[].numbers.getOrIncl n
  for r in rules.mitems:
    learnString r.name
    for c in r.rule.mitems:
      case c.kind
      of ckToken:
        c.token = parseToken(c.text)
        learnString c.text
        learnNumber c.token
      of ckTerminal:
        if c.term.kind == Token:
          c.term.token = parseToken(c.term.text)
          learnString c.term.text
          learnNumber c.term.token
        else:
          gram[].t.incl c.term
          case c.term.kind
          of Integer:
            learnNumber c.term.intVal.BiggestInt
          of Boolean:
            learnNumber c.term.boolVal.BiggestInt
          of Float:
            learnNumber cast[BiggestInt](c.term.floatVal)
          of String:
            learnString c.term.strVal
          of Symbol:
            learnString c.term.name
          else:
            discard

      of ckRule:
        if c.isReferential:
          inc nonterminals
        else:
          raise ValueError.newException "unexpected rule definition"
    withHeatMap gram:
      gram[].m[r.name] = 0
    try:
      gram[].p[r.name].add r.rule
    except KeyError:
      gram[].p[r.name] = @[r.rule]
  # shrink them
  gram[].strings = clone gram[].strings
  gram[].numbers = clone gram[].numbers
  info fmt"nonterminal references: {nonterminals}"
  info fmt"       total terminals: {gram[].t.card}"
  debug fmt"{gram[].strings.len} strings = {gram[].strings}"
  debug fmt"{gram[].numbers.len} numbers = {gram[].numbers}"

proc πFilling*[T](grammar: Grammar; genome: Genome): tuple[ast: Ast[T]; genome: Genome] {.inline.} =
  {.warning: "work around https://github.com/nim-lang/Nim/issues/19364".}
  let bug = πGE[T](grammar, genome)
  result = (ast: bug.ast, genome: genome)

proc πMap*[T](grammar: Grammar; genome: Genome): Program[T] {.inline.} =
  ## map a `genome` to a program using the given `grammar`
  {.warning: "work around https://github.com/nim-lang/Nim/issues/19364".}
  let bug = πFilling[T](grammar, genome)
  result = newProgram[T](bug.ast, bug.genome)
