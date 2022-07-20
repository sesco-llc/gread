import std/options
import std/math
import std/strutils

import pkg/cps

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
  float(s).formatFloat(format = ffDecimal, precision = 4)

proc percent*(f: float): string =
  (f * 100.0).formatFloat(format = ffDecimal, precision = 2) & "%"

proc isValid*(s: Score): bool =
  ## the score is a value we can get useful comparisons from
  if s.isNaN:
    false
  elif s.float in [-Inf, Inf]:
    false
  else:
    true

proc `$`*(g: Generation): string {.borrow.}
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
template debug*(args: varargs[untyped]): untyped =
  when debugging:
    echo args

when defined(greadProfile):
  import std/times
  import std/macros
  macro profile*(s: string; logic: untyped): untyped =
    let readTime = newCall bindSym"getTime"
    let readThread =
      when compileOption"threads":
        newCall bindSym"getThreadId"
      else:
        newLit"> "
    let clock = nskLet.genSym"clock"
    result = newStmtList()
    result.add newLetStmt(clock, readTime)
    result.add logic
    when compileOption"threads":
      result.add newCall(bindSym"debugEcho", readThread, newLit" ",
                         s, newLit" ", newCall(bindSym"inMilliseconds",
                         newCall(bindSym"-", readTime, clock)))
    else:
      result.add newCall(bindSym"debugEcho", s, newLit" ",
                         newCall(bindSym"inMilliseconds",
                               newCall(bindSym"-", readTime, clock)))
else:
  template profile*(s: string; logic: untyped): untyped = logic

when true:
  proc demandValid*[T](s: T) =
    mixin isValid
    if not s.isValid:
      raise Defect.newException "score of `{s}` is invalid"
  proc demandValid*[T](s: Option[T]) =
    mixin isValid
    if s.isSome:
      demandValid(get s)
else:
  template demandValid*(s: untyped): untyped =
    mixin isValid
    if not s.isValid:
      raise Defect.newException "score of `{s}` is invalid"
  template demandValid*[T](s: Option[T]): untyped =
    mixin isValid
    if s.isSome:
      demandValid(get s)
