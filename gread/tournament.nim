import std/algorithm
import std/logging

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
