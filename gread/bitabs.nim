## A BiTable is a table that can be seen as an optimized pair
## of (Table[LitId, Val], Table[Val, LitId]).

import std/hashes

type
  LitId* = distinct uint32

  BiTable*[T] = object
    vals: seq[T] # indexed by LitId
    keys: seq[LitId]  # indexed by hash(val)

proc nextTry(h, maxHash: Hash): Hash {.inline.} =
  result = (h + 1) and maxHash

template maxHash(t): untyped = high(t.keys)
template isFilled(x: LitId): bool = x.uint32 > 0'u32

proc `$`*(x: LitId): string {.borrow.}
proc `<`*(x, y: LitId): bool {.borrow.}
proc `<=`*(x, y: LitId): bool {.borrow.}
proc `==`*(x, y: LitId): bool {.borrow.}
proc hash*(x: LitId): Hash {.borrow.}


proc len*[T](t: BiTable[T]): int = t.vals.len

proc mustRehash(length, counter: int): bool {.inline.} =
  assert length > counter
  result = (length * 2 < counter * 3) or (length - counter < 4)

const
  idStart = 256 ## we'll retain this in case it becomes useful later

template idToIdx(x: LitId): int = x.int - idStart

proc hasLitId*[T](t: BiTable[T]; x: LitId): bool =
  let idx = idToIdx(x)
  result = idx >= 0 and idx < t.vals.len

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

proc getKeyId*[T](t: BiTable[T]; v: T): LitId =
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

proc `[]`*[T](t: var BiTable[T]; LitId: LitId): var T {.inline.} =
  let idx = idToIdx LitId
  assert idx <= t.vals.high
  result = t.vals[idx]

proc `[]`*[T](t: BiTable[T]; LitId: LitId): lent T {.inline.} =
  let idx = idToIdx LitId
  assert idx <= t.vals.high
  result = t.vals[idx]

proc hash*[T](t: BiTable[T]): Hash =
  ## as the keys are hashes of the values, we simply use them instead
  var h: Hash = 0
  for i, n in t.keys.pairs:
    h = h !& hash (i, n)
  result = !$h
