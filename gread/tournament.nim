import std/options
import std/heapqueue
import std/algorithm

export SortOrder

import gread/spec
import gread/population
import gread/programs
import gread/evolver

type
  Competitor*[T] = tuple     # sorting by
    valid: bool              # validity, then by
    score: Score             # score, then by
    len: int                 # program length
    index: int
    program: Program[T]

  Tourney*[T] = HeapQueue[Competitor[T]]

proc initTourney[T, V](evo: Evolver[T, V]; size: int): Tourney[T] =
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"

  # figure out the size of the tourney
  let size = max(1, min(evo.population.len, size))
  # get some data to test all the programs with
  let syms = evo.randomSymbols()
  while result.len < size:
    # pick a program; fetching the same program more than once is nbd
    var (i, p) = randomMember evo.population
    # score the program against the data
    let s = evo.score(syms, p)

    # take the opportunity to update the population
    let c = evo.scoreFromCache(p)
    if c.isSome:
      evo.population.scoreChanged(p, get c, index = some i)

    # resolve the score to a value we can sort with, maybe using parsimony
    let score =
      if s.isSome:
        if evo.tableau.useParsimony:
          penalizeSize(evo.population, get s, p.len)
        else:
          get s
     else:
       NaN
    # push into the queue; the early index prevents "sort by program"
    result.push (valid: s.isSome, score: score,
                 len: -p.len, index: i, program: p)

proc tournament*[T, V](evo: Evolver[T, V]; size: int;
                       order = Descending): Competitor[T] =
  ## find the fittest or least fit of a subset of the population
  var tourney = initTourney(evo, size)

  # we want the program with the highest score...
  if order == Descending:
    while tourney.len > 1:
      discard pop tourney

  if tourney.len > 0:
    result = pop tourney
  else:
    raise ValueError.newException "tournament unexpectedly empty"
