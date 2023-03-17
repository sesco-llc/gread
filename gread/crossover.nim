import std/options
import std/random

import gread/ast
import gread/grammar
import gread/genotype

# FIXME: optimize this after confirming the semantics

type
  Invention[T] = tuple[ast: Ast[T]; genome: Genome]
  ResultForm[T] = Option[Invention[T]]

template yieldInvention[T](gram: Grammar; genome: Genome): untyped =
  try:
    let bug = πFilling[T](gram, genome)
    yield some bug
  except ShortGenome:
    yield none Invention[T]

proc subtreeCrossover*[T](rng: var Rand; a, b: T): T =
  # ensure the first crossover point occurs in mutual coding space
  if a.len == 0 or b.len == 0:
    raise Defect.newException "attempt to crossover empty genome"
  # the second point can exceed the length of a shorter genome
  let n = rng.rand(0..min(a.high, b.high))
  let l = rng.rand(n..b.high)
  result =
    if n == 0:
      b[n..l]
    else:
      a[0..<n] & b[n..l]
  if l <= a.high:
    result.add a[l..a.high]

iterator geCrossover*[T](rng: var Rand; a, b: T): T =
  if a.len == 0 or b.len == 0:
    raise Defect.newException "attempt to crossover empty genome"
  let n = rng.rand(min(a.high, b.high))
  if n > 0:
    yield a[0..<n] & b[n..b.high]
    yield b[0..<n] & a[n..a.high]

iterator geCrossover*[T](rng: var Rand; gram: Grammar;
                         a, b: Genome): ResultForm[T] =
  ## perform GE crossover between two parents to form a new child
  for r in geCrossover(rng, a, b):
    yieldInvention[T](gram, r)

proc crossoverGroup*[T](rng: var Rand;
                        genomes: GenomeGroup[T]): GenomeGroup[T] =
  template a: T = genomes[0]
  template b: T = genomes[1]
  if a.len == 0 or b.len == 0:
    raise Defect.newException "attempt to crossover empty genome"
  if a.len > 2 and b.len > 2:  # no point to crossing tiny genomes
    let n = rng.rand(1..min(a.high-1, b.high-1))
    result.add: a[0..<n] & b[n..b.high]
    result.add: b[0..<n] & a[n..a.high]

iterator asymmetricCrossover*[T](rng: var Rand; a, b: T): T =
  if a.len == 0 or b.len == 0:
    raise Defect.newException "attempt to crossover empty genome"
  let x = rng.rand(a.high)
  let y = rng.rand(b.high)
  let n = a[0..<x] & b[y..b.high]
  let m = b[0..<y] & a[x..a.high]
  if n.len > 0:
    yield n
  if m.len > 0:
    yield m

proc asymmetricCrossoverGroup*[T](rng: var Rand;
                                  genomes: GenomeGroup[T]): GenomeGroup[T] =
  template a: T = genomes[0]
  template b: T = genomes[1]
  if a.len == 0 or b.len == 0:
    raise Defect.newException "attempt to crossover empty genome"
  let x = rng.rand(a.high)
  let y = rng.rand(b.high)
  let n = a[0..<x] & b[y..b.high]
  let m = b[0..<y] & a[x..a.high]
  if n.len > 0:
    result.add n
  if m.len > 0:
    result.add m

iterator asymmetricCrossover*[T](rng: var Rand; gram: Grammar;
                                 a, b: Genome): ResultForm[T] =
  ## perform GE crossover between two parents to form a new child
  for r in asymmetricCrossover(rng, a, b):
    yieldInvention[T](gram, r)

iterator randomAsymmetricCrossover*[T](rng: var Rand; gram: Grammar;
                                       a: Genome; size: int): ResultForm[T] =
  ## perform GE crossover between two parents to form a new child
  let b = randomGenome(size)
  for r in asymmetricCrossover(rng, a, b):
    yieldInvention[T](gram, r)

proc randomAsymmetricCrossoverGroup*[T](rng: var Rand;
                                        genomes: GenomeGroup[T]): GenomeGroup[T] =
  for genome in genomes.items:
    var inputs: GenomeGroup[T]
    inputs.add genome
    inputs.add randomGenome(rng, genome.len)
    for output in asymmetricCrossoverGroup(rng, inputs).items:
      result.add output

iterator randomSubtreeXover*[T](rng: var Rand; gram: Grammar;
                                a: Genome): ResultForm[T] =
  ## perform GE crossover between one parent and a random genome
  if a.len == 0:
    raise Defect.newException "received empty input genome"
  let b = randomGenome(rng, a.len)
  yieldInvention[T](gram, subtreeCrossover(rng, a, b))

iterator subtreeXover*[T](rng: var Rand; gram: Grammar;
                          a, b: Genome): ResultForm[T] =
  ## perform GE crossover between two parents to form a new child
  if a.len == 0 or b.len == 0:
    raise Defect.newException "received empty input genome"
  yieldInvention[T](gram, subtreeCrossover(rng, a, b))

iterator randomCrossover*[T](rng: var Rand; gram: Grammar;
                             a: Genome; size: int): ResultForm[T] =
  ## perform GE crossover between one parent and a random genome
  let b = randomGenome(rng, size)
  for genome in geCrossover(rng, a, b):
    yieldInvention[T](gram, genome)
