import std/algorithm
import std/random

export SortOrder

proc tournament*(rng: var Rand; bound: Natural; size: Positive;
                 order = Descending): int =
  var size = max(1, min(bound + 1, size))
  while size > 0:
    dec size
    result =
      case order
      of Ascending:
        min(result, rng.rand(bound))
      of Descending:
        max(result, rng.rand(bound))
