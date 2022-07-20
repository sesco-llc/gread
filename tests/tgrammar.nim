import std/sets

import pkg/balls

import gread/ast
import gread/grammar

import glang

suite "grammar":
  block:
    ## parse the reference grammar
    const example = """
     <start>          ::= <rule> | <rule> <start>
     <rule>           ::= <opt-whitespace> "<" <rule-name> ">" <opt-whitespace> "::=" <opt-whitespace> <expression> <line-end>
     <opt-whitespace> ::= " " <opt-whitespace> | ""
     <expression>     ::= <list> | <list> <opt-whitespace> "|" <opt-whitespace> <expression>
     <line-end>       ::= <opt-whitespace> <EOL> | <line-end> <line-end>
     <list>           ::= <term> | <term> <opt-whitespace> <list>
     <term>           ::= <literal> | "<" <rule-name> ">"
     <literal>        ::= '"' <text1> '"' | "'" <text2> "'"
     <text1>          ::= "" | <character1> <text1>
     <text2>          ::= '' | <character2> <text2>
     <character>      ::= <letter> | <digit> | <symbol>
     <letter>         ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
     <digit>          ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
     <symbol>         ::=  "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"
     <character1>     ::= <character> | "'"
     <character2>     ::= <character> | '"'
     <rule-name>      ::= <letter> | <rule-name> <rule-char>
     <rule-char>      ::= <letter> | <digit> | "-"
    """

    var gram: Grammar
    initGrammar(gram, parseToken, example)
    var seen: HashSet[string]
    for name, production in gram.pairs:
      check name != ""
      check production.len > 0
    for term in gram.terminals:
      case term.kind
      of String:
        seen.incl "s" & term.strVal
      of Integer:
        seen.incl "i" & $term.intVal
      of Float:
        seen.incl "f" & $term.floatVal
      of Symbol:
        seen.incl "y" & $term.name
      else:
        fail "unexpected terminal kind: " & $term.kind
    check "yD" in seen
    check "i3" in seen

  block:
    ## parse glang grammar
    var gram: Grammar
    initGrammar(gram, parseToken, glangGrammar)
    for name, production in gram.pairs:
      checkpoint name, " ", production
      if name == "terminate":
        checkpoint production
