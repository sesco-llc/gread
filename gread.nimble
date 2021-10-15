version = "0.0.1"
author = "disruptek"
description = "grammar-evolving algorithm designer"
license = "GPLv3"

requires "https://github.com/disruptek/lunacy < 1.0.0"
requires "https://github.com/nim-works/loony >= 0.1.6 & < 1.0.0"
#requires "https://github.com/haxscramper/htsparse < 1.0.0"

when not defined(release):
  requires "https://github.com/disruptek/balls >= 2.0.0 & < 4.0.0"

task test, "run tests for ci":
  when defined(windows):
    exec "balls.cmd"
  else:
    exec "balls"

task demo, "produce a demo":
  exec """demo docs/demo.svg "nim c --define:release --out=\$1 tests/test.nim""""

