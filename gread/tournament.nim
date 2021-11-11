import std/random
import std/options
import std/heapqueue
import std/algorithm

export SortOrder

import gread/spec
import gread/population
import gread/programs
import gread/evolver
import gread/data

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

  proc remover(competitors: var seq[Competitor[T]]; index: int) =
    ## used to update the population with a score change prior to removal
    let c = competitors[index]
    if c.program.isNil:
      raise
    let s = evo.scoreFromCache(c.program)
    let score =
      if s.isSome:
        get s
      else:
        NaN
    c.program.score = score
    c.program.zombie = score.isValid
    evo.population.scoreChanged(c.program, score, index = some index)
    del(competitors, index)

  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"

  # figure out the size of the tourney
  let size = max(1, min(evo.population.len, size))

  # select quantity N random programs; they must be unique.
  var victims: seq[Competitor[T]]     # programs we'll test
  while victims.len < size:
    var (i, p) = randomMember evo.population
    if p.isNil:
      raise
    victims.add (valid: not p.zombie, score: Score NaN,
                 len: p.len, index: i, program: p)

  when false:
    #echo "victims: ", victims.len
    for v in victims.items:
      if v.program.isNil:
        raise

  try:

    # randomize the selection of datapoints
    var samples = newSeqOfCap[int](evo.dataset.len)
    for i in evo.dataset.low .. evo.dataset.high:
      samples.add i
    shuffle samples

    var data: seq[SymbolSet[T, V]]       # datapoints we've tested
    while samples.len > 0 and victims.len > 1:
      when false:
        for v in victims.items:
          if v.program.isNil:
            raise
      # pick a novel datapoint we have not tested previously
      data.add evo.dataset[pop samples]

      # test all victims against the datapoint
      var i = 0
      while i <= victims.high and victims.len > 1:
        let p = victims[i].program
        if p.isNil:
          raise
        let s = evo.score(data[^1], p)
        if s.isSome and s.get.isValid:
          block:
            # re-score the program against all datapoints tested to date
            let s = evo.score(data, p)
            if s.isNone:
              # weird, but we must treat this as a very bad failure
              break
            else:
              victims[i].score = get s
              victims[i].valid = victims[i].score.isValid
            inc i
            continue

        # the program failed a fitone or a fitmany; toss it
        if order == Ascending:
          # a loser this bad is arguably "none more bad"
          result = victims[i]
          #echo "finally"
          return
        else:
          # just remove it from the tournament and continue on
          remover(victims, i)

      # if programs remain,
      if victims.len > 1:
        # sort the remainder, and
        sort(victims, order)
        # remove any losers
        while victims[^1].score != victims[0].score:
          remover(victims, victims.high)

    # the result will be the greatest winner
    result = victims[0]

    echo data.len

  finally:
    # remove the remaining victims to inform the population
    while victims.len > 0:
      remover(victims, victims.high)

proc tournament2*[T, V](evo: Evolver[T, V]; size: int;
                        order = Descending): Competitor[T] =
  ## (old version of tournament)
  var tourney = initTourney(evo, size)

  # we want the program with the highest score...
  if order == Descending:
    while tourney.len > 1:
      discard pop tourney

  if tourney.len > 0:
    result = pop tourney
  else:
    raise ValueError.newException "tournament unexpectedly empty"
