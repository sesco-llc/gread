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
