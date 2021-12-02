import std/math
import std/packedsets
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

const
  debugging = false
  exhaustive = false
template debug(args: varargs[untyped]): untyped =
  when debugging:
    echo args

import std/os
template think(final: untyped): untyped =
  when debugging:
    echo final.program
    sleep 10_000

proc tournament*[T, V](evo: Evolver[T, V]; size: int;
                       order = Descending): Competitor[T] =
  ## find the fittest or least fit of a subset of the population

  # FIXME: this code does not take parsimony into account

  proc remover(competitors: var seq[Competitor[T]]; i: int) =
    ## used to update the population with a score change prior to removal
    let c = competitors[i]
    # FIXME: optimization point
    let s = evo.scoreFromCache(c.program)
    #let s = evo.score(c.program)
    when debugging:
      let score =
        if s.isSome:
          get s
        else:
          NaN
      debug "rm ", i, " was ", c.program.score, " now ", score
    evo.population.scoreChanged(c.program, s, c.index)
    del(competitors, i)

  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"

  # figure out the size of the tourney
  let size = max(1, min(evo.population.len, size))

  # select quantity N random programs; they must be unique.
  var victims: seq[Competitor[T]]     # programs we'll test
  var seen: PackedSet[int]            # de-dupe victims
  var caches: float                   # average size of caches for victims
  while victims.len < size:
    var (i, p) = randomMember evo.population
    if p.isNil:
      raise
    if not seen.containsOrIncl i:
      caches += p.cacheSize.float
      victims.add (valid: not p.zombie, score: Score NaN,
                   len: p.len, index: i, program: p)
  caches = ceil(caches / victims.len.float)
  #caches = min(caches, evo.dataset.len.float / 10.0)
  #debug "victims:"
  #debug victims

  block byebye:

    # randomize the selection of datapoints
    var samples = newSeqOfCap[int](evo.dataset.len)
    for i in evo.dataset.low .. evo.dataset.high:
      samples.add i
    shuffle samples

    debug ""
    debug ""
    debug "sample order:"
    debug samples

    var data: seq[SymbolSet[T, V]]       # datapoints we've tested
    while samples.len > 0 and victims.len > 1: # and data.len < caches.int:
      # pick a novel datapoint we have not tested previously
      data.add evo.dataset[pop samples]

      # test all victims against the datapoint
      var i = 0
      debug "scoring against ", data[^1]
      while i <= victims.high and victims.len > 1:
        let p = victims[i].program
        if p.isNil:
          raise
        if p.cacheSize >= samples.len:
          inc i
        else:
          let s = evo.score(data[^1], p)
          if s.isSome:
            inc i
          else:
            debug "victim ", i, " failed score against ", data[^1]
            debug "victim ", i, " ", victims[i].program
            victims[i].score = NaN
            victims[i].valid = false

            debug "victim ", i, " is invalid"

            # the program failed a fitone or a fitmany; toss it
            if order == Ascending:
              # a loser this bad is arguably "none more bad"
              result = victims[i]
              break byebye
            else:
              # just remove it from the tournament and continue on
              remover(victims, i)

      i = 0
      while i <= victims.high and victims.len > 1:
        let p = victims[i].program
        # re-score the program against all datapoints tested to date
        let s = evo.score(data, p)
        if s.isSome:
          # update the competitor and move to the next victim
          debug "victim ", i, " was ", victims[i].score, " now ", get s
          victims[i].score = get s
          victims[i].valid = victims[i].score.isValid
          debug "victim ", i, " ", victims[i].program
          inc i
        else:
          remover(victims, i)

      when exhaustive:
        sort(victims, order)
      else:
        # if multiple programs remain,
        if victims.len > 1:
          # sort the remainder, and
          sort(victims, order)

          # remove any losers (or clones!)
          debug "removing victims worse than ", victims[0].score
          while victims.len > 1 and (victims[^1].score != victims[0].score or victims[^1].program.hash == victims[0].program.hash):
            remover(victims, victims.high)

      debug "loop done at ", i, " with data len ", data.len, " order ", order

    # the result will be the greatest winner
    result = victims[0]

  # remove the remaining victims to inform the population
  while victims.len > 0:
    remover(victims, victims.high)

  debug order, " program cache ", result.program.cacheSize, " average is ", Score caches
  debug "final result: ", result
  debug "actual score of winner: ", result.program.score
  think result
