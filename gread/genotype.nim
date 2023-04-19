import std/hashes
import std/logging
import std/monotimes
import std/random
import std/strformat

import pkg/adix/stat except Option

const
  greadWrapping* {.booldefine.} = false
  greadReportOperators* {.booldefine.} = false
  greadReportOperatorsInterval* {.intdefine.} = 10000

type
  ShortGenome* = object of ValueError
  Genome* = distinct string    ## a series of genes comprising an individual
  PC* = distinct int32         ## a program counter

  Genes = uint8 or uint16 or uint32 or uint64  ## optional types of genes

type
  GroupData[T] = array[4, T]
  GenomeGroup*[T] = object
    len: Natural
    data: GroupData[T]
  GenomeOperator*[T] = proc(rng: var Rand; genomes: GenomeGroup[T]): GenomeGroup[T]
  OperatorSpec[T] = object
    name*: string
    fn*: GenomeOperator[T]
    stat*: MovingStat[float32, uint32]
    timing*: MovingStat[float32, uint32]
    count*: int
    winners*: int
  GenomeOperatorSpec*[T] = ref OperatorSpec[T]
  GenomeWeight*[T] = tuple[spec: GenomeOperatorSpec[T]; weight: float64]

proc report(spec: OperatorSpec) =
  when greadReportOperators:
    let rate = spec.winners.float / spec.count.float * 100.0
    let map = spec.stat.mean * 100.0
    notice fmt"{spec.name} #{spec.count} win:{rate:>5.3f}% map:{map:>5.3f}% new:{spec.stat.n} time:{spec.timing.mean:>.0f}"

proc report*(spec: GenomeOperatorSpec) =
  report spec[]

proc `=destroy`[T](spec: var OperatorSpec[T]) =
  report spec

func operator*[T](name: string; fn: GenomeOperator[T]): GenomeOperatorSpec[T] =
  ## instantiate an operator spec for use in an operator weights alias method
  new result
  result.name = name
  result.fn = fn

proc high*[T](group: GenomeGroup[T]): Natural {.inline.} =
  if group.len == 0:
    raise IndexDefect.newException "group is empty"
  else:
    result = group.len - 1

func `[]`*[T](group: var GenomeGroup[T]; index: Natural): var T {.inline.} =
  if index <= group.high:
    result = group.data[index]
  else:
    raise IndexDefect.newException "bogus index"

func `[]`*[T](group: GenomeGroup[T]; index: Natural): T {.inline.} =
  if index <= group.high:
    group.data[index]
  else:
    raise IndexDefect.newException "bogus index"

func add*[T](group: var GenomeGroup[T]; item: sink T) {.inline.} =
  if item.len == 0:
    raise Defect.newException "empty genomes aren't terribly useful"
  group.data[group.len] = item
  inc group.len

iterator items*[T](group: GenomeGroup[T]): T =
  if group.len > 0:
    for index in 0..group.high:
      yield group.data[index]

func len*[T](group: GenomeGroup[T]): Natural {.inline.} =
  group.len

proc inc(spec: GenomeOperatorSpec) =
  when greadReportOperators:
    inc spec.count
    if spec.count mod greadReportOperatorsInterval == 0:
      report spec

template measureOperator*(op: GenomeOperatorSpec; logic: untyped): untyped =
  when greadReportOperators:
    var began = getMonoTime()
    try:
      logic
    finally:
      op.timing.push (getMonoTime() - began).inNanoseconds.float
      inc op
  else:
    logic

const
  EmptyGenome* = Genome""

proc `$`*(pc: PC): string {.borrow.}
proc `==`*(a, b: PC): bool {.borrow.}
proc inc(pc: var PC; n: int32) {.borrow.}

proc `$`*(geno: Genome): string {.borrow.}
proc low*(geno: Genome): int {.borrow.}
proc high*(geno: Genome): int {.borrow.}
proc len*(geno: Genome): int {.borrow.}
proc add(geno: var Genome; c: char) {.borrow.}     # nim bug;
proc add*(geno: var Genome; g: Genome) {.borrow.}  # the above is exported!
proc `&`*(a, b: Genome): Genome {.borrow.}
proc `[]`*[T, U: Ordinal](genome: Genome; hs: HSlice[T, U]): Genome =
  ## essentially a `.borrow.` which works around a nim bug
  let size = hs.b.ord - hs.a.ord
  if size <= 0:
    return EmptyGenome
  when false:
    result = newString(size)
    copyMem(addr result.string[0], unsafeAddr genome.string[hs.a.ord], size)
  else:
    result = Genome genome.string[hs]

proc `[]=`*(geno: var Genome; index: int; ch: char) =
  geno.string[index] = ch

proc canRead*[T: Genes](geno: Genome; pc: PC; count = 1): bool {.inline.} =
  ## true if there remain at least `count` genes between the
  ## program counter `pc` and the end of the genome
  pc.int <= geno.len - (sizeof(T) * count)

proc read*[T: Genes](geno: Genome; pc: var PC): T

proc read*[T: Genes](geno: Genome; pc: var PC; into: var T) {.inline.} =
  ## read a gene of `T` into `into` from genome `geno` and
  ## advance the program counter `pc`
  if canRead[T](geno, pc):
    into =
      when T is uint8:
        try:
          uint8(string(geno)[int pc])
        finally:
          inc(pc, sizeof T)
      elif T is uint16:
        uint16(read[uint8](geno, pc) shl 8) or read[uint8](geno, pc)
      elif T is uint32:
        uint32(read[uint16](geno, pc) shl 16) or read[uint16](geno, pc)
      else:
        uint64(read[uint32](geno, pc) shl 32) or read[uint32](geno, pc)
  else:
    raise IndexDefect.newException "ran out of genes for " & $T

proc read*[T: Genes](geno: Genome; pc: var PC): T =
  ## read a gene of `T` from genome `geno` and
  ## advance the program counter `pc`
  read(geno, pc, result)

proc randomGenome*(rng: var Rand; size: int): Genome =
  ## using the provided random state,
  ## generate a random genome of the given size
  result = Genome newString(size)
  for i in result.low .. result.high:
    result[i] = rng.rand(int char.high).char

proc toString*(geno: Genome): string =
  string geno

proc fromString*(str: string): Genome =
  Genome str

proc toBytes*(genome: Genome): seq[byte] =
  setLen(result, genome.len)
  for index in 0..result.high:
    result[index] = byte genome.string[index]

proc hash*(geno: Genome): Hash =
  hash geno.string

proc `==`*(a, b: Genome): bool =
  a.string == b.string
