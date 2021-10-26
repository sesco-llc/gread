import std/sequtils
import std/hashes

import gread/ast

type
  SymbolSet*[T, V] = object
    hash: Hash
    values: seq[DataPoint[T, V]]

  DataPoint*[T, V] = object
    name*: string
    kind*: AstKind
    value*: V

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

proc initDataPoint*[T, V](name: string; value: V;
                          kind = akNone): DataPoint[T, V] =
  DataPoint[T, V](name: name, value: value, kind: kind)

proc initSymbolSet*[T, V](values: openArray[DataPoint[T, V]]): SymbolSet[T, V] =
  ## convert an openArray of DataPoints into a suitable SymbolSet
  result.values = @values
  result.hash = hash result.values

proc initSymbolSet*[T, V](values: openArray[(string, V)]): SymbolSet[T, V] =
  ## convert an openArray of (name, value) pairs into a suitable SymbolSet
  mixin initDataPoint
  var points = newSeqOfCap[DataPoint[T, V]](values.len)
  for name, value in values.items:
    points.add:
      initDataPoint[T, V](name, value)
  result = initSymbolSet[T, V](points)

iterator pairs*[T, V](ss: SymbolSet[T, V]): tuple[key: string, val: V] =
  for point in ss.values.items:
    yield (key: point.name, val: point.value)

iterator items*[T, V](ss: SymbolSet[T, V]): DataPoint[T, V] =
  for point in ss.values.items:
    yield point

iterator keys*[T, V](ss: SymbolSet[T, V]): string =
  for point in ss.values.items:
    yield point.name

proc values*[T, V](ss: SymbolSet[T, V]): lent seq[DataPoint[T, V]] =
  ss.values

proc hash*(ss: SymbolSet): Hash = ss.hash
