version = "0.0.6"
author = "disruptek"
description = "grammar-evolving algorithm designer"
license = "GPLv3"

requires "https://github.com/disruptek/lunacy >= 0.0.8 & < 1.0.0"
requires "https://github.com/nim-works/loony >= 0.1.8 & < 1.0.0"
requires "https://github.com/nim-works/cps >= 0.5.0 & < 1.0.0"
requires "https://github.com/c-blake/adix >= 0.2.1 & < 0.3.0"
requires "https://github.com/c-blake/cligen < 1.5.20"  # for adix/stat
requires "https://github.com/disruptek/redis < 1.0.0"
requires "https://github.com/disruptek/frosty >= 3.0.0 & < 4.0.0"
requires "https://github.com/haxscramper/htsparse >= 0.1.10 & < 1.0.0"
requires "https://github.com/haxscramper/hmisc >= 0.14.5 & < 1.0.0"

when defined(greadGraph):
  requires "https://github.com/Vindaar/ggplotnim > 0.4.0 & < 1.0.0"

when not defined(release):
  requires "https://github.com/disruptek/balls >= 2.0.0 & < 4.0.0"

task test, "run tests for ci":
  when defined(windows):
    exec "balls.cmd --define:useMalloc --threads:on --gc:arc"
  else:
    exec "balls --define:useMalloc --threads:on --gc:arc --define:greadTreeSitter"

task demo, "produce a demo":
  exec """demo docs/demo.svg "nim c --define:release --out=\$1 tests/test.nim""""

