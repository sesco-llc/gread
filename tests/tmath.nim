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
    check almostEqual(hoeffding(50, 0.2'f32), 0.03663127869367599, 0)
    check almostEqual(hoeffding(50, 0.2'f64), 0.03663127777746836, 0)
    check almostEqual(hoeffding(50, 0.2), 0.03663127, 500_000_000)

  block:
    ## normal distribution
    let a = [1.0, 2.0, 3.0, 4.0, 5.0]
    let c = [2.0, 1.0, 2.2, 1.6, 0.6]
    checkpoint a.normal(3.5)
    check almostEqual(a.normal(3.5), 0.2650035323440286)
    checkpoint c.normal(0.8)
    check almostEqual(c.normal(0.8), 0.3500389871146745)

  block:
    ## parsimony expectations
    let scores = [-1.0, -2.0, -3.0, -4.0, -5.0]
    let lengths = [5.0, 10.0, 15.0, 21.0, 27.0]
    let parsimony = covariance(lengths, scores) / variance(lengths)
    check parsimony < 0.0
