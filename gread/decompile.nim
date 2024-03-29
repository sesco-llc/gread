import std/hashes
import std/json
import std/options
import std/random
import std/sequtils
import std/tables

import gread/ast
import gread/grammar
import gread/genotype
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

proc `{}`(x: openArray[Ordinal|enum]; index: int): int =
  if index > x.high:
    0
  else:
    ord(x[index])

proc hamming*[T](x1, x2: seq[T]; normalize = false): float =
  var total: int
  for k in 0..x1.high:
    total += (x1{k} != x2{k}).ord
  result =
    if normalize:
      total.float / x1.len.float
    else:
      total.float

proc jaccard*[T](x1, x2: seq[T]; normalize = false): float =
  var total_min, total_max: int
  for col in 0..max(x1.high, x2.high):
    total_min += min(x1{col}, x2{col})
    total_max += max(x1{col}, x2{col})
  result =
    if total_max == 0:
      0.0
    else:
      1.0 - (total_min.float / total_max.float)

proc bag*[T](a, b: T): int =
  ## bag distance between two openArray-ish things
  let one = a.toCountTable
  let two = b.toCountTable
  result = max(sum(one - two), sum(two - one))

proc decompiler*[T](d: var T; gram: Grammar; source: string;
                    rng: Rand = randState()): auto =
  mixin decompiler
  var tableau = defaultTableau
  tableau.maxPopulation = 10
  decompiler(d, defaultTableau, gram, source, rng = rng)
