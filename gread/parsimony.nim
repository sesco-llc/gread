import std/algorithm
import std/deques
import std/math
import std/options
import std/packedsets
import std/sequtils
import std/strutils

import pkg/adix/stat except Option

import gread/spec
import gread/programs
import gread/maths

import gread/population

const
  moreParsimony = false
  greadFixedParsimony {.strdefine.} = "nan"
  greadClampedParsimony {.strdefine.} = "nan"

let fixedParsimony = parseFloat greadFixedParsimony
let clampedParsimony = abs: parseFloat greadClampedParsimony

when false:
  type
    PopLike[T] = concept c
      c.push(T)
      c.pop() is T
      c.pop(T)
      c.min is T
      c.max is T
      c.len is Natural
      #c.high is Natural
      #c[Natural] is T
      c.select(Positive) is T
      c.rank(T) is Positive
      (T < T) is bool
      (T == T) is bool
      T.len is Natural
      T.score is Option[SomeFloat]
      for n in c.items:
        n is T

type
  Parsimony*[T] = object of RootObj
    members*: T
    parsimony: float
    lengths: MovingStat[float, uint32]
    scores: MovingStat[float, uint32]

proc downscale*(s: Option[float]): float =
  result =
    if s.isSome:
      get s
    else:
      -Inf
  if result.isNaN:
    result = -Inf

proc covariance[T](parsimony: Parsimony[T]): float =
  mixin score
  if parsimony.lengths.n == 0:
    return NaN
  for item in parsimony.members.values:
    let s = parsimony.members.score(item)
    if s.isSome:
      result += (get(s).float - parsimony.scores.mean) *
                (item.len.float - parsimony.lengths.mean)
  result /= parsimony.lengths.n.float

proc len*[T](parsimony: Parsimony[T]): Natural =
  mixin len
  parsimony.members.len

proc high*[T](parsimony: Parsimony[T]): Natural =
  mixin high
  parsimony.members.len.Natural - 1

proc min*[K, V](parsimony: Parsimony[K]): V =
  mixin min
  result = parsimony.members.min

proc max*[K, V](parsimony: Parsimony[K]): V =
  mixin max
  result = parsimony.members.max

proc select*[T](parsimony: Parsimony[T]; rank: Positive): auto =
  mixin select
  let bug = parsimony.members.select(rank)
  result = bug.val

proc `[]`*[T](parsimony: Parsimony[T]; index: Natural): auto =
  parsimony.select(1 + index)

proc score*[K, V](parsimony: Parsimony[K]; item: V): Option[float] =
  mixin score
  result = parsimony.members.score(item)
  if result.isSome:
    if not parsimony.parsimony.isNaN:
      # if parsimony is negative,
      #    reduce the score of longer programs,
      # else,
      #    raise the score of longer programs
      result = some: get(result) + parsimony.parsimony * item.len.float

proc recompute*[T](parsimony: var Parsimony[T]) =
  mixin score
  mixin sort
  mixin values
  when greadFixedParsimony != "nan":
    parsimony.parsimony = fixedParsimony
    return
  reset parsimony.lengths
  reset parsimony.scores
  if parsimony.members.len == 0:
    parsimony.parsimony = NaN
  else:
    for item in parsimony.members.values:
      let s = item.score
      if s.isSome:
        parsimony.scores.push get(s).float
        parsimony.lengths.push item.len.float
    parsimony.parsimony = parsimony.covariance / parsimony.lengths.variance
    when greadClampedParsimony != "nan":
      parsimony.parsimony = clamp(parsimony.parsimony, -clampedParsimony .. clampedParsimony)
    sort parsimony

proc push*[K, V](parsimony: var Parsimony[K]; item: V) =
  ## insert an item, ranking it according to parsimony
  mixin insert
  let s = parsimony.score(item)
  if parsimony.members.insert(s, item):
    if s.isSome:
      recompute parsimony

proc pop*[K, V](parsimony: var Parsimony[K]; item: V) =
  ## remove an item, as ranked according to parsimony
  mixin remove
  let s = parsimony.score(item)
  if parsimony.members.remove(s, item):
    when moreParsimony:
      if s.isSome:
        recompute parsimony

proc pop*[T](parsimony: var Parsimony[T]): auto =
  let bug = parsimony.members.min
  doAssert parsimony.members.remove(bug.key)
  result = bug.val

func paintMetrics*(metrics: var PopMetrics; population: Parsimony) =
  ## reset validity, score, and parsimony metrics in the population; O(n)
  clear metrics.validity
  clear metrics.scores
  clear metrics.caches
  clear metrics.lengths
  metrics.size = 0
  for program in population.items:
    inc metrics.size
    if program.isValid:
      metrics.validity.push 1.0
      if program.score.isValid:
        metrics.scores.push program.score
    else:
      metrics.validity.push 0.0
    metrics.lengths.push program.len.float
    if program.core == metrics.core:
      metrics.ages.push float(int program.generation)
