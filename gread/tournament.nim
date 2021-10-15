import std/math
import std/heapqueue
import std/algorithm

export SortOrder

import gread/spec
import gread/population

type
  Competitor*[T] = tuple  # sorting by
    score: Score             # score, then by
    len: int                 # program length
    index: int
    program: Program[T]

  Tourney*[T] = HeapQueue[Competitor[T]]

proc initTourney[T](pop: Population[T]; size: int): Tourney[T] =
  if pop.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"

  let size = max(1, min(pop.len, size))
  while result.len < size:
    # fetching the same program more than once is nbd
    var (i, p) = randomMember pop
    result.push (score: pop.score(p), len: -p.len, index: i, program: p)

proc tournament*[T](pop: Population[T]; size: int;
                    order = Descending): Competitor[T] =
  ## find the fittest or least fit of a subset of the population

  var tourney = initTourney(pop, size)

  # we want the program with the highest score...
  if order == Descending:
    while tourney.len > 1:
      discard tourney.pop()

  if tourney.len > 0:
    result = tourney.pop()
  else:
    raise ValueError.newException "tournament unexpectedly empty"
