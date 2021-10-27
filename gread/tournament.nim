import std/heapqueue
import std/algorithm

export SortOrder

import gread/spec
import gread/population
import gread/programs
import gread/manager

type
  Competitor*[T] = tuple  # sorting by
    valid: bool              # validity, then by
    score: Score             # score, then by
    len: int                 # program length
    index: int
    program: Program[T]

  Tourney*[T] = HeapQueue[Competitor[T]]

proc initTourney[T](man: Manager[T]; size: int): Tourney[T] =
  if man.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"

  let size = max(1, min(man.population.len, size))
  while result.len < size:
    # fetching the same program more than once is nbd
    var (i, p) = randomMember man.population
    let s = man.score(p)
    result.push (valid: s.isValid, score: s,
                 len: -p.len, index: i, program: p)

proc tournament*[T](man: Manager[T]; size: int;
                    order = Descending): Competitor[T] =
  ## find the fittest or least fit of a subset of the population
  var tourney = initTourney(man, size)

  # we want the program with the highest score...
  if order == Descending:
    while tourney.len > 1:
      discard pop tourney

  if tourney.len > 0:
    result = pop tourney
  else:
    raise ValueError.newException "tournament unexpectedly empty"
