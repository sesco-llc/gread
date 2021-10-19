import gread/ast

type
  Primitives*[T] = ref object
    functions*: seq[Function[T]]
    constants*: seq[Terminal[T]]
    inputs*: seq[Terminal[T]]
    outputs*: seq[Terminal[T]]

proc newPrimitives*[T](): Primitives[T] =
  new Primitives[T]

func terminals*[T](c: Primitives[T]): seq[Terminal[T]] =
  c.constants & c.inputs & c.outputs
