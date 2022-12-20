import std/sequtils
import std/hashes

import gread/ast
import gread/audit

type
  SymbolSet*[T, V] = object       ## group of symbolName=value associations
    hash: Hash
    values: seq[DataPoint[T, V]]

  DataPoint*[T, V] = object       ## single symbolName=value association
    name*: string
    value*: V

converter toValue*[T, V](point: DataPoint[T, V]): V =
  point.value

proc len*(ss: SymbolSet): int = ss.values.len

proc `$`*(dp: DataPoint): string =
  mixin `$`
  result.add dp.name
  result.add "="
  result.add $dp.value

proc `$`*(ss: SymbolSet): string =
  mixin `$`
  result.add "["
  result.add mapIt(ss.values, $it).join(" ")
  result.add "]"

proc initDataPoint*[T, V](name: string; value: V): DataPoint[T, V] =
  ## initialize a datapoint using symbol name and value
  result = DataPoint[T, V](name: name, value: value)
  compact result.name
  doAssert result.name.capacity == name.len

proc initSymbolSet*[T, V](values: openArray[DataPoint[T, V]]): SymbolSet[T, V] =
  ## convert an openArray of DataPoints into a suitable SymbolSet
  result.values = @values
  result.hash = hash result.values
  doAssert result.values.capacity == values.len

proc initSymbolSet*[T, V](values: openArray[(string, V)]): SymbolSet[T, V] =
  ## convert an openArray of (name, value) pairs into a suitable SymbolSet
  mixin initDataPoint
  var points = newSeqOfCap[DataPoint[T, V]](values.len)
  for name, value in values.items:
    points.add:
      initDataPoint[T, V](name, value)
  result.values = points
  result.hash = hash result.values

type
  NamePoint[T, V] = tuple  # only for pairs iteration; no export
    key: string
    val: DataPoint[T, V]

iterator pairs*[T, V](ss: SymbolSet[T, V]): NamePoint[T, V] =
  ## yield (key: symbol name, val: datapoint) from the set
  for point in ss.values.items:
    yield (key: point.name, val: point)

iterator items*[T, V](ss: SymbolSet[T, V]): DataPoint[T, V] =
  ## yield datapoints from the set
  for point in ss.values.items:
    yield point

iterator keys*[T, V](ss: SymbolSet[T, V]): string =
  ## yield symbol names from the set
  for point in ss.values.items:
    yield point.name

iterator values*[T, V](ss: ptr SymbolSet[T, V]): ptr DataPoint[T, V] =
  ## when provided a ptr, yield ptrs to the datapoints within
  var i = 0
  var hi = ss[].values.high
  while i < hi:
    yield addr ss[].values[i]
    inc i

proc values*[T, V](ss: SymbolSet[T, V]): lent seq[DataPoint[T, V]] =
  ss.values

proc hash*(ss: SymbolSet): Hash = ss.hash

proc `[]`*[T, V](ss: SymbolSet[T, V]; name: string): DataPoint[T, V] =
  ## convenience to fetch, by symbol name, a datapoint from the set
  for key, value in ss.pairs:
    if name == key:
      return value
