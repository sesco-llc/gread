import std/strutils
import std/os

proc cap(s: string): int =
  {.emit: ["if(s.p != 0) result = s.p->cap;"].}

proc size*(s: string): int {.inline.} =
  s.cap + 1 + sizeof(int)

proc compact*(s: var string) {.inline.} =
  if s.capacity != s.len:
    var x = newStringOfCap s.len
    x.add s
    s = x
  #doAssert s.capacity == s.len, "cap of " & $s.capacity & " vs len " & $s.len

when defined(useMalloc) and not defined(valgrind):
  {.pragma: malloc, header: "<malloc.h>", importc.}
  type
    mallinfo* = object
      arena*: csize_t        ## non-mmapped bytes allocated
      ordblks*: csize_t      ## number of free chunks
      smblks*: csize_t       ## number of free fastbin blocks
      hblks*: csize_t        ## number of mmapped regions
      hblkhd*: csize_t       ## bytes allocated in mmapped regions
      usmblks*: csize_t      ## (unused)
      fsmblks*: csize_t      ## bytes in freed fastbin blocks
      uordblks*: csize_t     ## total allocated bytes
      fordblks*: csize_t     ## total free bytes
      keepcost*: csize_t     ## top-most, releasable bytes

  proc mallinfo2*(): mallinfo {.importc: "mallinfo2".}
  proc mallopt*(param: cint; value: cint): cint {.importc: "mallopt".}
  proc mallocOption*(param: cint; value: int) =
    if 0 == mallopt(param.cint, value.cint):
      raiseOSError(osLastError(), "arguments: $# $#" % [$param, $value])

  let underValgrind* = mallinfo2().arena == 0

  if false: #not underValgrind:
    mallocOption(-8, 1) # arenas=1
    mallocOption(1, 0) # fastbin size=0
    mallocOption(-4, 1) # mmap max
    mallocOption(-3, 4*1024*1024) # mmap threshold

  template memoryArena*(): untyped =
    if underValgrind:
      0
    else:
      mallinfo2().arena.int

  template memoryUsed*(): untyped =
    if underValgrind:
      0
    else:
      mallinfo2().uordblks.int

  import std/rlocks
  var L: RLock
  initRLock L
  template assertHeapGrowthLessThan*(count: int; logic: typed): untyped =
    var a, b: MallocInfo
    var growth: int
    if underValgrind:
      logic
    else:
      withRLock L:
        block here:
          a = mallinfo2()
          if a.arena == 0:
            logic
            break here
          try:
            logic
          finally:
            b = mallinfo2()
            growth = b.uordblks.int - a.uordblks.int
            if growth >= count:
              debugEcho a
              debugEcho b
              raise AssertionDefect.newException "heap grew by " & $growth & "; more than " & $count
            elif growth != 0:
              ##debugEcho "                          heap ", growth
else:
  const underValgrind* = false
  template assertHeapGrowthLessThan*(count: int; logic: typed): untyped =
    {.warning: "useMalloc not enabled".}
    logic

  template memoryArena*(): untyped =
    {.warning: "useMalloc not enabled".}
    0

  template memoryUsed*(): untyped =
    {.warning: "useMalloc not enabled".}
    0
