import std/math

import pkg/balls

import gread/maths

suite "math":
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

  block:
    ## hoeffding's inequality
    checkpoint hoeffding(12, 0.2'f32)
    checkpoint hoeffding(12, 0.2'f64)
    check almostEqual(hoeffding(12, 0.2'f32), 0.7657857537269592, 0)
    check almostEqual(hoeffding(12, 0.2'f64), 0.7657857719502239, 0)
    check almostEqual(hoeffding(12, 0.2), 0.7657857, 500_000_000)
    checkpoint hoeffding(50, 0.2'f32)
    checkpoint hoeffding(50, 0.2'f64)
    check almostEqual(hoeffding(50, 0.2'f32), 0.0366312600672245, 0)
    check almostEqual(hoeffding(50, 0.2'f64), 0.03663127777746833, 0)
    check almostEqual(hoeffding(50, 0.2), 0.03663127, 500_000_000)
