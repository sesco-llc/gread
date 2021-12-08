import std/hashes

## A BiTable is a table that can be seen as an optimized pair
## of (Table[LitId, Val], Table[Val, LitId]).
##
## this impl is copied from $nim/compiler/ic/bitabs and tweaked
##

type
  LitId* = distinct uint32
  BiTable*[T] = object
    vals: seq[T]      # indexed by LitId; length is thus that of the table
    keys: seq[LitId]  # indexed by hash(val); length is thus >= that of vals

const
  idStart = 256 ## we currently use this to add significance to
  ## an operand value of 0, which we sem as having no literal

converter toInt32*(n: LitId): int32 = n.int32

template idToIdx(x: LitId): int = x.int - idStart
template isFilled(x: LitId): bool = x.uint32 > 0'u32
template maxHash(t): untyped = high(t.keys)

proc nextTry(h, maxHash: Hash): Hash {.inline.} =
  result = (h + 1) and maxHash

proc `$`*(x: LitId): string {.borrow.}
proc `<`*(x, y: LitId): bool {.borrow.}
proc `<=`*(x, y: LitId): bool {.borrow.}
proc `==`*(x, y: LitId): bool {.borrow.}
proc hash*(x: LitId): Hash {.borrow.}

proc len*[T](t: BiTable[T]): int = t.vals.len

proc mustRehash(length, counter: int): bool {.inline.} =
  assert length > counter
  result = (length * 2 < counter * 3) or (length - counter < 4)

when false:
  #[

  omitting these until a need presents itself

  ]#
  proc contains*[T](t: BiTable[T]; x: LitId): bool =
    let idx = idToIdx(x)
    result = idx >= 0 and idx <= t.vals.high

  proc `[]`[T](t: BiTable[T]; v: T): LitId =
    let origH = hash(v)
    var h = origH and maxHash(t)
    if t.keys.len != 0:
      while true:
        let litId = t.keys[h]
        if not isFilled(litId):
          break
        if t.vals[idToIdx t.keys[h]] == v:
          return litId
        h = nextTry(h, maxHash(t))
    raise KeyError.newException "key not found"

proc enlarge[T](t: var BiTable[T]) =
  var n: seq[LitId]
  newSeq(n, len(t.keys) * 2)
  swap(t.keys, n)
  for i in 0..high(n):
    let eh = n[i]
    if isFilled(eh):
      var j = hash(t.vals[idToIdx eh]) and maxHash(t)
      while isFilled(t.keys[j]):
        j = nextTry(j, maxHash(t))
      t.keys[j] = move n[i]

proc getOrIncl*[T](t: var BiTable[T]; v: T): LitId =
  let origH = hash(v)
  var h = origH and maxHash(t)
  if t.keys.len != 0:
    while true:
      let litId = t.keys[h]
      if not isFilled(litId): break
      if t.vals[idToIdx t.keys[h]] == v: return litId
      h = nextTry(h, maxHash(t))
    # not found, we need to insert it:
    if mustRehash(t.keys.len, t.vals.len):
      enlarge(t)
      # recompute where to insert:
      h = origH and maxHash(t)
      while true:
        let litId = t.keys[h]
        if not isFilled(litId): break
        h = nextTry(h, maxHash(t))
  else:
    setLen(t.keys, 16)
    h = origH and maxHash(t)

  result = LitId(t.vals.len + idStart)
  t.keys[h] = result
  t.vals.add v

proc `[]`*[T](t: var BiTable[T]; id: LitId): var T {.inline.} =
  let idx = idToIdx id
  assert idx <= t.vals.high, "idx " & $idx
  result = t.vals[idx]

proc `[]`*[T](t: BiTable[T]; id: LitId): lent T {.inline.} =
  let idx = idToIdx id
  assert idx <= t.vals.high, "idx " & $idx
  result = t.vals[idx]

proc hash*[T](t: BiTable[T]): Hash =
  ## as the keys are hashes of the values, we simply use them instead
  var h: Hash = 0
  for i, n in t.keys.pairs:
    h = h !& hash (i, n)
  result = !$h
