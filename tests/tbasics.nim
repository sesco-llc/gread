import std/math

import pkg/balls

import gread/ast
import gread/spec
import gread/fennel


suite "basic machinery":
  block:
    ## ast node
    var a = initAst fun"+"
    check a.len == 1
    check a[0].kind == Node
    check $a == "(+ )"

  block:
    ## ast node plus leaf
    var a = initAst fun"+"
    a.add: term 2.0
    check a.len == 2
    check $a == "(+ 2.0)"
    check a[1].kind == Leaf

  block:
    ## ast node plus leaves
    var a = initAst fun"+"
    a.add: term 2.0
    a.add: term 3.0
    check a.len == 3
    check $a == "(+ 2.0 3.0)"
    check $a[1] == "2.0"
    check $a[2] == "3.0"
    check a[2].leaf.kind == Constant
    check a[2].leaf.ck == Float

  block:
    ## tree with branches (shape A)
    var a = initAst fun"+"
    a.add: term 2.0
    a.add: asAst(fun"-", term 6.0, term 1.0)
    a.add: term 3.0
    check a.len == 6
    check $a[1] == "2.0"
    check $a[2] == "(- 6.0 1.0)"
    check $a[3] == "6.0"
    check $a[4] == "1.0"
    check $a[5] == "3.0"

  block:
    ## tree with branches (shape B)
    var a = initAst fun"+"
    a.add: asAst(fun"-", term 6.0, term 1.0)
    a.add: term 3.0
    check a.len == 5
    check $a[1] == "(- 6.0 1.0)"
    check $a[2] == "6.0"
    check $a[3] == "1.0"
    check $a[4] == "3.0"

  block:
    ## tree with branches (shape C)
    var a = initAst fun"+"
    a.add: term 3.0
    a.add: asAst(fun"-", term 6.0, term 1.0)
    check a.len == 5
    check $a[1] == "3.0"
    check $a[2] == "(- 6.0 1.0)"
    check $a[3] == "6.0"
    check $a[4] == "1.0"

  block:
    ## supporting math
    let a = [1.0, 2.0, 3.0, 4.0, 5.0]
    let b = [2.0, 3.0, 4.0, 5.0, 6.0]
    let r2 = sqrt 2.0
    check "is stats.nim really worse?":
      variance(a) == 2.0
      variance(b) == 2.0
      covariance(a, b) == 2.0
      stddev(a) == r2
      correlation(a, b) == (2.0 / (r2*r2))
