import std/hashes
import std/random

const
  greadWrapping* {.booldefine.} = false

type
  ShortGenome* = object of ValueError
  Genome* = distinct string    ## a series of genes comprising an individual
  PC* = distinct int32         ## a program counter

  Genes = uint8 or uint16 or uint32 or uint64  ## optional types of genes

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
  pc.int <= geno.high - (sizeof(T) * count)

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

#converter toString*(geno: Genome): string =
#  string geno

converter fromString*(str: string): Genome =
  Genome str

proc hash*(geno: Genome): Hash =
  hash geno.string

proc `==`*(a, b: Genome): bool =
  a.string == b.string
