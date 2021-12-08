import std/math

proc avg*[T: SomeOrdinal](a: openArray[T]): T {.inline.} =
  sum(a) div T(a.len)

proc avg*[T: not SomeOrdinal](a: openArray[T]): T {.inline.} =
  sum(a) / T(a.len)

proc covariance*(a, b: openArray[float]): float =
  if a.len == b.len:
    let a1 = avg a
    let b1 = avg b
    for index in 0..<a.len:
      result += (a[index] - a1) * (b[index] - b1)
    result /= a.len.float
  else:
    raise ValueError.newException "inputs have unequal girth"

proc variance*[T](a: openArray[T]): T =
  let mean = avg a
  for item in a.items:
    result += (item - mean) * (item - mean)
  when T is SomeOrdinal:
    result = result div T(a.len)
  else:
    result = result / T(a.len)

proc stddev*[T](a: openArray[T]): T {.inline.} =
  sqrt variance(a)

proc correlation*(a, b: openArray[float]): float {.inline.} =
  covariance(a, b) / (stddev(a) * stddev(b))

proc ss*[T](a: openArray[T]): T {.inline.} =
  ## sum of squares
  for i in 0..<a.len:
    result += a[i] * a[i]

proc ss*[T](a, b: openArray[T]): T {.inline.} =
  ## sum of squares
  if a.len == b.len:
    for i in 0..<a.len:
      result += (a[i] - b[i]) * (a[i] - b[i])
  else:
    raise ValueError.newException "inputs have unequal girth"

proc hoeffding*[T](n: int; e: T): T =
  ## hoeffding's inequality; for `n` samples, the probability
  ## that `e`, a deviation from expected value, holds
  T(2.0) * exp(T(-2.0) * e * e * T(n))

proc normal*[T](x: T; mean: T; deviation: T): T =
  result = 1.0 / (deviation * sqrt(2.0 * PI))
  result *= exp(-0.5 * pow((x - mean) / deviation, 2))

template normal*[T](a: openArray[T]; x: T): T =
  normal(x, avg a, stddev a)
