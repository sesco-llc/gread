import std/times
import std/macros
import std/math
import std/strutils

type
  Score* = distinct float

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

macro profile*(s: string; logic: untyped): untyped =
  when defined(danger):
    result = logic
  else:
    let readTime = newCall bindSym"getTime"
    let readThread = newCall bindSym"getThreadId"
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
