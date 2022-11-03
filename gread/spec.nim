import std/math
import std/options
import std/strutils
import std/tables

import pkg/cps

when compileOption"threads":
  when not defined(useMalloc):
    {.warning: "--define:useMalloc or suffer slow allocator performance".}

when defined(greadSlowTable):
  import std/tables
  export tables
  type GreadTable*[K, V] = Table[K, V]
  func initGreadTable*[K, V](table: var GreadTable[K, V]; initialSize = defaultInitialSize) =
    table = initTable[K, V](initialSize=initialSize)
else:
  import pkg/adix/lptabz
  export lptabz
  type GreadTable*[K, V] = LPTabz[K, V, void, 0]
  proc initGreadTable*[K, V](table: var GreadTable[K, V]; initialSize = defaultInitialSize) =
    table.init(initialSize=initialSize)
    table.clear()

when defined(greadLargeCache):
  type GreadCache*[K, V] = GreadTable[K, V]
  proc initGreadCache*(cache: var GreadCache; initialSize = defaultInitialSize) =
    initGreadTable(cache, initialSize=initialSize)
else:
  import pkg/lrucache
  export lrucache
  type GreadCache*[K, V] = LruCache[K, V]
  func initGreadCache*[K, V](cache: var GreadCache[K, V]; initialSize = defaultInitialSize) =
    cache = newLruCache[K, V](initialSize)

type
  Score* = distinct float
  CoreSpec* = Option[CoreId]
  CoreId* = int
  Generation* = distinct int
  C* = ref object of Continuation

converter toFloat*(s: Score): float = float s
converter toScore*[T: float or float64](f: T): Score = Score f

proc `+`*(a, b: Score): Score {.borrow.}
proc `-`*(a, b: Score): Score {.borrow.}
proc `*`*(a, b: Score): Score {.borrow.}
proc `/`*(a, b: Score): Score {.borrow.}
proc `$`*(s: Score): string =
  "s" & float(s).formatFloat(format = ffDecimal, precision = 4)
proc ff*(s: float): string =
  float(s).formatFloat(format = ffDecimal, precision = 6)

proc percent*(f: float): string =
  (f * 100.0).formatFloat(format = ffDecimal, precision = 2) & "%"

proc isValid*(s: Score): bool =
  ## the score is a value we can get useful comparisons from
  const negativeInf = -Inf
  if s.isNaN:
    false
  elif s.float == Inf or s.float == negativeInf:
    false
  else:
    true

proc `$`*(g: Generation): string = "g" & $int(g)
proc `<`*(a, b: Generation): bool {.borrow.}
proc `==`*(a, b: Generation): bool {.borrow.}
proc inc*(g: var Generation; n: int = 1) {.borrow.}
proc `mod`*(a: Generation; b: int): int = a.int.mod b
converter toInt*(g: Generation): int = g.int

#proc get*(cs: CoreSpec): int = get Option[int](cs)
#proc isSome*(cs: CoreSpec): bool = isSome Option[int](cs)
#proc isNone*(cs: CoreSpec): bool = isNone Option[int](cs)

proc `$`*(cs: CoreSpec): string =
  if cs.isSome:
    $get(cs)
  else:
    "-"

const
  debugging* = defined(greadDebug) and not defined(release)

when defined(greadProfile):
  import std/times
  import std/monotimes
  import std/macros
  macro profile*(s: string; logic: typed): untyped =
    let readTime = newCall bindSym"getMonoTime"
    let readThread =
      when compileOption"threads":
        newCall bindSym"getThreadId"
      else:
        newLit"> "
    let clock = nskLet.genSym"clock"
    result = newStmtList()
    result.add newLetStmt(clock, readTime)
    var value = nskLet.genSym"value"
    if getType(logic).strVal == "void":
      result.add logic
    else:
      result.add newLetStmt(value, logic)
    when compileOption"threads":
      result.add newCall(bindSym"debugEcho", readThread, newLit" ",
                         s, newLit" ", newCall(bindSym"inMilliseconds",
                         newCall(bindSym"-", readTime, clock)))
    else:
      result.add newCall(bindSym"debugEcho", s, newLit" ",
                         newCall(bindSym"inMilliseconds",
                               newCall(bindSym"-", readTime, clock)))
    if getType(logic).strVal != "void":
      result.add newStmtList(value)
    #else:
    #  result.add nnkDiscardStmt.newTree(newStmtList(value))
else:
  template profile*(s: string; logic: untyped): untyped = logic

template demandValid*[T](s: T) =
  mixin isValid
  if not s.isValid:
    raise Defect.newException "score of `{s}` is invalid"

template demandValid*[T](s: Option[T]) =
  mixin isValid
  if s.isSome:
    demandValid(get s)

when defined(greadMemoryAudit):
  import std/logging
  import std/strformat

  import pkg/grok/mem
  import pkg/grok/kute
  import pkg/grok/resources
  type
    Auditor = GreadTable[string, int]

  proc prettyDump*(auditor: Auditor) =
    for stage, memory in auditor.pairs:
      notice fmt"{stage:>40} {Kute(memory):>8}"

  proc audit*(auditor: Auditor; stage: string;
              mem: int; total: int) =
    let name =
      when compileOption"threads":
        $getThreadId()
      else:
        "(no thread)"
    notice fmt"{name} {stage} memory cost {Kute(mem)} now {Kute(auditor[stage])}"
    auditor.prettyDump()

  var auditor*: Auditor
  initGreadTable auditor
  template memoryAudit*(stage: string; logic: untyped): untyped =
    var thread: ThreadResources
    var mem = -thread.maxResidentBytes
    try:
      logic
    finally:
      sample thread
      block:
        let total = thread.maxResidentBytes
        mem += total
        if mem != 0:
          when defined(greadSlowTable):
            mgetOrDefault(auditor, stage, 0).inc(mem)
          else:
            # NOTE: this crap because adix is dumb
            try:
              auditor[stage] = auditor[stage] + mem
            except KeyError:
              auditor[stage] = mem
          auditor.audit(stage, mem, total)
else:
  template memoryAudit*(stage: string; logic: untyped): untyped = logic
