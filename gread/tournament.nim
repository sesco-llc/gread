import std/hashes
import std/math
import std/packedsets
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

import std/os
template think(final: untyped): untyped =
  when debugging:
    echo final.program
    #sleep 4_000

proc remover[T, V](evo: var Evolver[T, V];
                   competitors: var seq[Competitor[T]]; i: int) =
  ## used to update the population with a score change prior to removal
  let c = competitors[i]
  # FIXME: optimization point
  let s =
    # XXX: the problem here is that the score may well be -0.0 if the (few?)
    #      datapoints yield an impressive score
    when defined(greadFast):
      evo.scoreFromCache(c.program)
    else:
      evo.score(c.program)
  when debugging:
    let score =
      if s.isSome:
        get s
      else:
        NaN
    debug "rm ", i, " t-score ", c.score, " was ", c.program.score, " now ", score
  evo.population.scoreChanged(c.program, s, c.index)
  del(competitors, i)

proc discharge(evo: var Evolver; c: Competitor) =
  ## a modern remover
  scoreChanged(evo.population, c.program,
               evo.scoreFromCache(c.program), c.index)
  if evo.cacheSize(c.program) == evo.dataset.len:
    maybeResetFittest(evo.population, c.program)

proc tournament3*[T, V](evo: var Evolver[T, V]; size: int;
                       order = Descending): Competitor[T] =
  ## v3 baby
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"
  if size < 1:
    raise ValueError.newException:
      "cannot run a tournament with less than one competitor"

  # figure out the size of the tourney
  let size = max(1, min(evo.population.len, size))

  # the winner of each bout fights again
  var victim: Competitor[T]
  var seen: PackedSet[int]           # de-dupe fighters by index;
  # we're counting unique programs, not unique members!
  while seen.len < size:
    var (i, p) = randomMember(evo.population, evo.rng)
    if not seen.containsOrIncl i:
      victim = (valid: p.isValid, score: Score NaN,
                len: p.len, index: i, program: p)
      if result.program.isNil:
        # it's our first time through the loop, so we'll establish
        # the defender
        result = victim
      else:
        # we have an encumbent; see what's better when
        profile "confident comparo":
          let cmp =
            confidentComparison(evo, victim.program, result.program)
        if cmp == -1 and order == Ascending:
          discharge(evo, result)
          result = victim
        elif cmp == 1 and order == Descending:
          discharge(evo, result)
          result = victim
        else:
          discharge(evo, victim)

  # reset the score of the winner only if necessary
  discharge(evo, result)
  result.valid = result.program.isValid
  result.score = result.program.score

  debug ""
  debug "tournament result: ", result
  debug "actual score of winner: ", result.program.score
  debug "order ", order
  think result

proc tournament2*[T, V](evo: var Evolver[T, V]; size: int;
                        order = Descending): Competitor[T] =
  ## find the fittest or least fit of a subset of the population
  mixin strength
  if evo.population.len < 1:
    raise ValueError.newException:
      "cannot run a tournament with empty population"

  # figure out the size of the tourney
  let size = max(1, min(evo.population.len, size))

  # select quantity N random programs; they must be unique.
  var victims: seq[Competitor[T]]     # programs we'll test
  var seen: PackedSet[int]            # de-dupe victims
  if size == evo.population.len:
    for i, p in evo.population.pairs:
      victims.add (valid: not p.zombie, score: Score NaN,
                   len: p.len, index: i, program: p)
  else:
    while victims.len < size:
      var (i, p) = randomMember(evo.population, evo.rng)
      if not seen.containsOrIncl i:
        victims.add (valid: not p.zombie, score: Score NaN,
                     len: p.len, index: i, program: p)

  block byebye:
    # randomize the selection of datapoints
    var samples = evo.randomDataIndexes()

    debug ""
    debug ""
    debug "sample order:"
    debug samples

    var indices: PackedSet[int]          # indices we've tested
    while samples.len > 0 and victims.len > 1:
      # pick a novel datapoint we have not tested previously
      let index = pop samples
      indices.incl index

      # test all victims against the datapoint
      var i = 0
      debug "scoring against ", evo.dataset[index]
      while i <= victims.high and victims.len > 1:
        let p = victims[i].program

        when false: # let's not confuse the issue here
          if evo.cacheSize(p) >= samples.len:
            inc i
            continue # added so the when makes sense

        else:

          let s = evo.score(index, p)
          if s.isSome:
            inc i
          else:
            debug "victim ", i, " failed score against ", evo.dataset[index]
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
              evo.remover(victims, i)

      i = 0
      while i <= victims.high and victims.len > 1:
        let p = victims[i].program
        # re-score the program against all datapoints tested to date
        let s = evo.score(addr indices, p)
        if s.isSome:
          # update the competitor and move to the next victim
          debug "victim ", i, " was ", victims[i].score, " now ", get s
          # adjust the score according to population parsimony
          victims[i].score =
            evo.population.score(strength(get s), victims[i].len)
          victims[i].valid = victims[i].score.isValid
          debug "victim ", i, " ", victims[i].program
          inc i
        else:
          evo.remover(victims, i)

      # if multiple programs remain,
      if victims.len > 1:
        # sort the remainder, and
        sort(victims, order)

        # remove any losers (or clones!)
        debug "removing victims worse than ", victims[0].score
        while victims.len > 1 and (victims[^1].score != victims[0].score or victims[^1].program.hash == victims[0].program.hash):
          evo.remover(victims, victims.high)

      debug "loop done at ", i, " with data len ", data.len, " order ", order

    # the result will be the greatest winner
    result = victims[0]

  # remove the remaining victims to inform the population
  while victims.len > 0:
    evo.remover(victims, victims.high)

  debug "final result: ", result
  debug "actual score of winner: ", result.program.score
  think result

template tournament*[T, V](evo: Evolver[T, V]; size: int;
                           order = Descending): Competitor[T] =
  when defined(greadFast):
    tournament3(evo, size, order)
  else:
    tournament2(evo, size, order)
