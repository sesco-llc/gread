import std/random

type
  Genome* = distinct string    ## a series of genes comprising an individual
  PC* = distinct int32         ## a program counter

  Genes = uint8 or uint16 or uint32 or uint64  ## optional types of genes

proc `$`*(pc: PC): string {.borrow.}
proc inc(pc: var PC; n: int32) {.borrow.}

proc high*(geno: Genome): int {.borrow.}
proc len*(geno: Genome): int {.borrow.}
proc add(geno: var Genome; c: char) {.borrow.}
proc `[]`*[T, U: Ordinal](geno: Genome; hs: HSlice[T, U]): Genome =
  ## essentially a `.borrow.` which works around a nim bug
  Genome geno.string[hs]

proc canRead*[T: Genes](geno: Genome; pc: PC; count = 1): bool =
  ## true if there remain at least `count` genes `T between the
  ## program counter `pc` and the end of the genome
  pc.int <= geno.len - (sizeof(T) * count)

proc read*[T: Genes](geno: Genome; pc: var PC): T

proc read*[T: Genes](geno: Genome; pc: var PC; into: var T) =
  ## read a gene of `T` into `into` from genome `geno` and
  ## advance the program counter `pc`
  if canRead[T](geno, pc, count = 1):
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
    raise IndexDefect.newException "ran out of genes"

proc read*[T: Genes](geno: Genome; pc: var PC): T =
  ## read a gene of `T` from genome `geno` and
  ## advance the program counter `pc`
  read(geno, pc, result)

proc randomGenome*(size: int): Genome =
  ## generate a random genome of the given size
  result = Genome newStringOfCap(size)
  while result.len < size:
    result.add rand(int char.high).char
