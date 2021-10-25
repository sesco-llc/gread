import std/times
import std/macros
import std/math
import std/strutils

type
  Score* = distinct float
  Generation* = distinct int

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

macro profile*(s: string; logic: untyped): untyped =
  when defined(danger):
    result = logic
  else:
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
