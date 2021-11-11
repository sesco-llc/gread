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

  block byebye:

    # randomize the selection of datapoints
    var samples = newSeqOfCap[int](evo.dataset.len)
    for i in evo.dataset.low .. evo.dataset.high:
      samples.add i
    shuffle samples

    var data: seq[SymbolSet[T, V]]       # datapoints we've tested
    while samples.len > 0 and victims.len > 1:
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
          break byebye
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

  # remove the remaining victims to inform the population
  while victims.len > 0:
    remover(victims, victims.high)
