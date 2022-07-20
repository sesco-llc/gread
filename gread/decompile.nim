import std/strutils
import std/tables
import std/hashes
import std/sequtils
import std/random
import std/json
import std/options

import gread/ast
import gread/grammar
import gread/genotype
import gread/evolver
import gread/programs
import gread/tableau
import gread/data

type
  G = string

type
  Invention[T] = tuple[ast: Ast[T]; genome: Genome]
  ResultForm[T] = Option[Invention[T]]

proc hash[T](a: Ast[T]; n: AstNode[T]): Hash =
  ## perform a stable hash of node `n` within ast `a`
  mixin isSymbol
  mixin isStringLit
  mixin isNumberLit
  mixin isParent

  var h: Hash = 0
  h = h !& hash(n.kind)
  if n.isSymbol or n.isStringLit:
    h = h !& hash(a.strings[LitId n.operand])
  elif n.isNumberLit:
    h = h !& hash(a.numbers[LitId n.operand])
  elif not n.isParent and n.operand != 0:  # NOTE: assume it's a token
    discard
  result = !$h

proc toCountTable(a: Ast): CountTable[Hash] =
  ## count the occurrences of nodes in an ast
  for n in a.nodes.items:
    result.inc a.hash(n)

proc `-`[T](a, b: CountTable[T]): CountTable[T] =
  ## diff two CountTables
  for key, value in a.pairs:
    result.inc(key, value - b.getOrDefault(key, 0))

proc sum[T](t: CountTable[T]): int =
  ## sum the counters in a CountTable
  for value in t.values:
    result.inc value

proc bag*[T](a, b: T): int =
  ## bag distance between two openArray-ish things
  let one = a.toCountTable
  let two = b.toCountTable
  result = max(sum(one - two), sum(two - one))

proc strength*(score: G): float =
  #const expectedMapping = "(if (> 2.0 b ) 2.0 (- 0.0 a ) )"
  const
    expectedMapping = "(if (or (not= 0.0 2.0 ) (not= 1.0 1.0 ) ) 0.0 0.5 )"
  # FIXME: switch to ast comparo
  let codeAsCharacters = expectedMapping
  let gs = score
  result = -bag(gs, codeAsCharacters).float

proc isValid*(g: G): bool =
  g.len > 0

proc decompiler*[T](d: var T; tableau: Tableau; gram: Grammar;
                    source: string; rng: Rand = randState()): Evolver[T, G] =
  let codeAsCharacters = source.toSeq
  proc strength(g: G): float =
    let gs = g.toSeq
    result = bag(gs, codeAsCharacters).float

  proc fitone(d: T; data: SymbolSet[T, G]; p: Program[T]): Option[G] =
    result = some $p

  proc fitmany(d: T; iter: iterator(): (ptr SymbolSet[T, G], ptr G);
               p: Program[T]): Option[G] =
    for symbols, s in iter():
      return some $p

  var evo: Evolver[T, G]
  initEvolver(evo, d, tableau, rng)
  evo.operators = {
    geCrossover[T, string]:     200.0,
    geMutation[T, string]:      100.0,
    randomCrossover[T, string]:   1.0,
  }
  evo.grammar = gram
  evo.fitone = fitone
  evo.fitmany = fitmany
  evo.dataset = @[initSymbolSet[T, G]([("source", source)])]
  evo.population = evo.randomPop()
  result = evo

proc decompiler*[T](d: var T; gram: Grammar; source: string;
                    rng: Rand = randState()): auto =
  var tableau = defaultTableau
  tableau.maxPopulation = 10
  decompiler(d, defaultTableau, gram, source, rng = rng)
