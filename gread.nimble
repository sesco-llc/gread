version = "0.0.42"
author = "disruptek"
description = "grammar-evolving algorithm designer"
license = "GPLv3"

requires "https://github.com/disruptek/lunacy >= 0.0.9 & < 1.0.0"
requires "https://github.com/disruptek/grok >= 0.6.3 & < 1.0.0"
requires "https://github.com/nim-works/loony >= 0.1.8 & < 1.0.0"
requires "https://github.com/disruptek/insideout >= 0.0.13 & < 1.0.0"
requires "https://github.com/nim-works/arc < 1.0.0"
requires "https://github.com/disruptek/adix >= 0.4.2 & < 1.0.0"
requires "https://github.com/disruptek/redis >= 0.0.4 & < 1.0.0"
requires "https://github.com/disruptek/frosty >= 3.0.0 & < 4.0.0"
requires "https://github.com/haxscramper/htsparse >= 0.1.10 & < 1.0.0"
requires "https://github.com/haxscramper/hmisc >= 0.14.5 & < 1.0.0"
requires "https://github.com/treeform/sysinfo >= 0.2.1 & < 1.0.0"
requires "https://github.com/jackhftang/lrucache.nim.git < 2.0.0"

when not defined(release):
  requires "https://github.com/disruptek/balls >= 3.9.0 & < 4.0.0"

task demo, "produce a demo":
  exec """demo docs/demo.svg "nim c --define:release --out=\$1 tests/test.nim""""
