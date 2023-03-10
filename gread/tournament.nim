import std/algorithm
import std/random

export SortOrder

proc tournament*(rng: var Rand; bound: Natural; size: Positive;
                 order = Descending): Natural =
  var size = size - 1
  result = rng.rand(bound)
  while size > 0:
    dec size
    result =
      case order
      of Ascending:
        min(result, rng.rand(bound))
      of Descending:
        max(result, rng.rand(bound))
