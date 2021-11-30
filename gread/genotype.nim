import std/random

type
  Genotype* = distinct string
  PC* = distinct int32

  GenoReadTypes = uint8 or uint16 or uint32 or uint64

proc `$`*(pc: PC): string {.borrow.}
proc inc(pc: var PC; n: int32) {.borrow.}

proc high*(geno: Genotype): int {.borrow.}
proc len*(geno: Genotype): int {.borrow.}
proc add(geno: var Genotype; c: char) {.borrow.}

proc canRead*[T: GenoReadTypes](geno: Genotype; pc: PC; count = 1): bool =
  pc.int <= geno.high - (sizeof(T) * count)

proc read*[T: GenoReadTypes](geno: Genotype; pc: var PC): T

proc read*[T: GenoReadTypes](geno: Genotype; pc: var PC; into: var T) =
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

proc read*[T: GenoReadTypes](geno: Genotype; pc: var PC): T =
  read(geno, pc, result)

proc randomGenotype*(size: int): Genotype =
  ## generate a random genotype of the given size
  result = Genotype newStringOfCap(size)
  while result.len < size:
    result.add rand(int char.high).char
