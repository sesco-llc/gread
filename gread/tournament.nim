import std/algorithm

import gread/spec
import gread/programs

export SortOrder

type
  Competitor*[T] = tuple     # sorting by
    valid: bool              # validity, then by
    score: Score             # score, then by
    len: int                 # program length
    index: int
    program: Program[T]

when debugging:
  import std/os
  template think*(final: untyped): untyped =
    echo final.program
    #sleep 4_000
else:
  template think*(final: untyped): untyped = discard

