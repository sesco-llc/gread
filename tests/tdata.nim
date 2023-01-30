import std/json

import pkg/balls

import gread

import ./glang

proc main =
  suite "data":
    block:
      ## testing data operations
      let stuff =
        initSymbolSet[G, JsonNode]:
          {
            "a": newJInt 4,
            "b": newJString "hello",
            "c": newJFloat 3.5,
          }
      check stuff.len == 3
      var count = stuff.len
      for name, point in stuff.pairs:
        check name in ["a", "b", "c"]
        case point.value.kind
        of JInt:      check point.getInt == 4
        of JString:   check point.getStr == "hello"
        of JFloat:    check point.getFloat == 3.5
        else: fail"unexpected json kind"
        dec count
      check count == 0, "missing some items"

      for point in stuff.items:
        case point.value.kind
        of JInt:      check $point == "a=4"
        of JString:   check $point == "b=\"hello\""
        of JFloat:    check $point == "c=3.5"
        else: fail"unexpected json kind"

main()
