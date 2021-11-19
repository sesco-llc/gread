import pkg/adix/lptabz

import gread/aliasmethod
import gread/ast

type
  ComponentKind = enum ckToken, ckRule, ckTerminal
  Component[T] = object
    case kind: ComponentKind
    of ckToken:
      token: int16
    of ckRule:
      rule: Production[T]
    of ckTerminal:
      term: Terminal[T]

  Production[T] = seq[Component[T]]

  GrammarObj[T] = object
    s: Component[T]
    p: LPTab[Component[T], Production[T]]
    t: seq[Terminal[T]]
    n: seq[Function[T]]
  Grammar[T] = ptr GrammarObj[T]

  Genotype = seq[int]

proc initGrammar[T](gram: Grammar[T]) =
  if not gram.isNil:
    dealloc gram
  gram = cast[Grammar[T]](allocShared0 sizeof(GrammarObj[T]))
  if gram.isNil:
    raise ValueError.newException "unable to create grammar"

proc toAst[T](prod: Production[T]): Ast[T] =
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

proc πGE[T](gram: Grammar; geno: Genotype): Ast[T] =
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
