type
  Genotype* = distinct string
  PC* = distinct int32

  GenoReadTypes = uint8 or uint16 or uint32 or uint64

proc high*(geno: Genotype): int {.borrow.}
proc len*(geno: Genotype): int {.borrow.}

#converter toInt(pc: PC): int {.used.} = int pc

proc canRead*[T: GenoReadTypes](geno: Genotype; pc: PC; count = 1): bool =
  pc <= geno.high - (sizeof(T) * count)

proc read*[T: GenoReadTypes](geno: Genotype; pc: var PC; into: var T) =
  if canRead(geno, pc):
    into =
      when T is uint8:
        uint8(geno[pc])
      elif T is uint16:
        uint16(read[uint8](geno, pc) shl 8) and read[uint8](geno, pc)
      elif T is uint32:
        uint32(read[uint16](geno, pc) shl 16) and read[uint16](geno, pc)
      else:
        uint64(read[uint32](geno, pc) shl 32) and read[uint32](geno, pc)
    inc(pc, sizeof T)
  else:
    raise IndexDefect.newException "ran out of genes"

proc read*[T: GenoReadTypes](geno: Genotype; pc: var PC): T =
  read(geno, pc, result)
