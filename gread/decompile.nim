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

proc hamming[T](x1, x2: seq[T]; normalize = false): float =
  let cols = x1.len
  var total: int
  for k in 0..<cols:
    total += (x1[k] != x2[k]).ord
  result =
    if normalize:
      total.float / cols.float
    else:
      total.float

proc jaccard*[T](x1, x2: seq[T]; normalize = false): float =
  let cols = x1.len
  var
    total_min: int
    total_max: int
  for k in 0..<cols:
    total_min += min(x1[k], x2[k]).ord
    total_max += max(x1[k], x2[k]).ord
  result =
    if total_max == 0:
      0.0
    else:
      1.0 - (total_min.float / total_max.float)

proc strength*(score: G): float =
  #const expectedMapping = "(if (> 2.0 b ) 2.0 (- 0.0 a ) )"
  const
    expectedMapping = "(if (or (not= 0.0 2.0 ) (not= 1.0 1.0 ) ) 0.0 0.5 )"
  let codeAsCharacters = toSeq expectedMapping
  let gs = score.toSeq
  result =
    if gs.len == codeAsCharacters.len:
      #-hamming(gs, codeAsCharacters, normalize = true)
      -jaccard(gs, codeAsCharacters, normalize = true)
    else:
      -(200.0 * abs(gs.len - codeAsCharacters.len).float)

proc isValid*(g: G): bool =
  g.len > 0

proc decompiler*[T](d: var T; tableau: Tableau; gram: Grammar;
                    source: string; rng: Rand = randState()): Evolver[T, G] =
  let codeAsCharacters = source.toSeq
  proc strength(g: G): float =
    let gs = g.toSeq
    result = jaccard(gs, codeAsCharacters, normalize = true)

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
