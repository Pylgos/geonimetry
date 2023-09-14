import std/[macros, strformat, math]
import ./matrices

{.push inline.}

type
  Vector*[N: static int; Scalar = float] = Matrix[N, 1, Scalar]
  
  Vector2f* = Vector[2, float32]
  Vector2d* = Vector[2, float64]
  Vector3f* = Vector[3, float32]
  Vector3d* = Vector[3, float64]

func initVector*[N: static int, Scalar](data: sink array[N, Scalar]): Vector[N, Scalar] =
  for i in 0..<N:
    result[i, 0] = data[i]

proc vector2*[T](x: T = 0, y: T = 0): Vector[2, T] = initVector([x, y])
proc vector2f*(x = 0'f32, y = 0'f32): Vector[2, float32] = vector2(x, y)
proc vector2d*(x = 0'f64, y = 0'f64): Vector[2, float64] = vector2(x, y)

proc vector3*[T](x: T = 0, y: T = 0, z: T = 0): Vector[3, T] = initVector([x, y, z])
proc vector3f*(x = 0'f32, y = 0'f32, z = 0'f32): Vector[3, float32] = vector3(x, y, z)
proc vector3d*(x = 0'f64, y = 0'f64, z = 0'f64): Vector[3, float64] = vector3(x, y, z)

template `[]`*[N: static int; Scalar](v: Vector[N, Scalar], i: int): Scalar =
  v.data[i][0]

template `[]`*[N: static int; Scalar](v: var Vector[N, Scalar], i: int): var Scalar =
  v.data[i][0]

template `[]=`*[N: static int; Scalar](v: var Vector[N, Scalar], i: int, val: Scalar) =
  v.data[i][0] = val

{.experimental: "dotOperators".}

macro `.`*[N: static int, Scalar](v: Vector[N, Scalar], swizzle: untyped{nkIdent}): untyped =
  const swizzleChars = ['x', 'y', 'z', 'w']
  let s = swizzle.strVal
  if s.len == 1:
    error(fmt"invalid swizzle '{s}'", swizzle)
  else:
    result = newStmtList()
    let tmp = genSym(nskLet, "tmp")
    result.add newLetStmt(tmp, v)
    result.add nnkCall.newTree(
      nnkBracketExpr.newTree(
        bindSym"initVector",
        newLit s.len,
        nnkDotExpr.newTree(tmp, ident("Scalar"))),
      nnkBracket.newNimNode())
    for i, c in s:
      let idx = swizzleChars.find(c)
      if idx == -1 or N <= idx:
        var info = swizzle.lineInfoObj()
        info.column += i
        swizzle.setLineInfo(info)
        error(fmt"invalid swizzle '{c}'", swizzle)
      result[1][1].add newCall(bindSym"[]", tmp, newLit idx)

macro `.=`*[N: static int, Scalar](v: var Vector[N, Scalar], swizzle: untyped{nkIdent}, rhs: Vector[N, Scalar] | array[N, Scalar]): untyped =
  const swizzleChars = ['x', 'y', 'z', 'w']
  let s = swizzle.strVal
  if s.len == 1:
    error(fmt"invalid swizzle '{s}'", swizzle)
  else:
    result = newStmtList()
    for i, c in s:
      let idx = swizzleChars.find(c)
      if idx == -1 or N <= idx:
        var info = swizzle.lineInfoObj()
        info.column += i
        swizzle.setLineInfo(info)
        error(fmt"invalid swizzle '{c}'", swizzle)
      result.add newCall(bindSym"[]=", v, newLit idx, newCall(bindSym"[]", rhs, newLit i))

template swiz*(v: Vector, swizzle: untyped{nkIdent}): untyped =
  `.`(v, swizzle)

template swiz*[N: static int, Scalar](v: Vector[N, Scalar], swizzle: untyped{nkIdent}, rhs: Vector[N, Scalar] | array[N, Scalar]): untyped =
  `.=`(v, swizzle, rhs)

func x*[Scalar](v: Vector[1, Scalar]): Scalar {.inline.} = v[0]
func x*[Scalar](v: var Vector[1, Scalar]): var Scalar {.inline.} = v[0]
func x*[Scalar](v: Vector[2, Scalar]): Scalar {.inline.} = v[0]
func x*[Scalar](v: var Vector[2, Scalar]): var Scalar {.inline.} = v[0]
func x*[Scalar](v: Vector[3, Scalar]): Scalar {.inline.} = v[0]
func x*[Scalar](v: var Vector[3, Scalar]): var Scalar {.inline.} = v[0]
func x*[Scalar](v: Vector[4, Scalar]): Scalar {.inline.} = v[0]
func x*[Scalar](v: var Vector[4, Scalar]): var Scalar {.inline.} = v[0]

func y*[Scalar](v: Vector[2, Scalar]): Scalar {.inline.} = v[1]
func y*[Scalar](v: var Vector[2, Scalar]): var Scalar {.inline.} = v[1]
func y*[Scalar](v: Vector[3, Scalar]): Scalar {.inline.} = v[1]
func y*[Scalar](v: var Vector[3, Scalar]): var Scalar {.inline.} = v[1]
func y*[Scalar](v: Vector[4, Scalar]): Scalar {.inline.} = v[1]
func y*[Scalar](v: var Vector[4, Scalar]): var Scalar {.inline.} = v[1]

func z*[Scalar](v: Vector[3, Scalar]): Scalar {.inline.} = v[2]
func z*[Scalar](v: var Vector[3, Scalar]): var Scalar {.inline.} = v[2]
func z*[Scalar](v: Vector[4, Scalar]): Scalar {.inline.} = v[2]
func z*[Scalar](v: var Vector[4, Scalar]): var Scalar {.inline.} = v[2]

func w*[Scalar](v: Vector[4, Scalar]): Scalar {.inline.} = v[3]
func w*[Scalar](v: var Vector[4, Scalar]): var Scalar {.inline.} = v[3]

func `x=`*[Scalar](v: var Vector[1, Scalar], val: Scalar) {.inline.} = v[0] = val
func `x=`*[Scalar](v: var Vector[2, Scalar], val: Scalar) {.inline.} = v[0] = val
func `x=`*[Scalar](v: var Vector[3, Scalar], val: Scalar) {.inline.} = v[0] = val
func `x=`*[Scalar](v: var Vector[4, Scalar], val: Scalar) {.inline.} = v[0] = val

func `y=`*[Scalar](v: var Vector[2, Scalar], val: Scalar) {.inline.} = v[1] = val
func `y=`*[Scalar](v: var Vector[3, Scalar], val: Scalar) {.inline.} = v[1] = val
func `y=`*[Scalar](v: var Vector[4, Scalar], val: Scalar) {.inline.} = v[1] = val

func `z=`*[Scalar](v: var Vector[3, Scalar], val: Scalar) {.inline.} = v[2] = val
func `z=`*[Scalar](v: var Vector[4, Scalar], val: Scalar) {.inline.} = v[2] = val

func `w=`*[Scalar](v: var Vector[4, Scalar], val: Scalar) {.inline.} = v[3] = val

proc dot*[N: static int; ScalarA, ScalarB](a: Vector[N, ScalarA], b: Vector[N, ScalarB]): auto =
  result = default(typeof(default(ScalarA) * default(ScalarB)))
  for i in 0..<N:
    result += a[i] * b[i]

template `*`*[N: static int; ScalarA, ScalarB](a: Vector[N, ScalarA], b: Vector[N, ScalarB]): auto = dot(a, b)

proc cross*[ScalarA, ScalarB](a: Vector[0, ScalarA], b: Vector[0, ScalarB]): auto =
  result = Vector[0, productT(a, b)]()

proc cross*[ScalarA, ScalarB](a: Vector[1, ScalarA], b: Vector[1, ScalarB]): auto =
  result = initVector[1, productT(ScalarA, ScalarB)]([0])

proc cross*[ScalarA, ScalarB](a: Vector[3, ScalarA], b: Vector[3, ScalarB]): auto =
  result = initVector[3, productT(ScalarA, ScalarB)]([
    a[1]*b[2] - a[2]*b[1],
    a[2]*b[0] - a[0]*b[2],
    a[0]*b[1] - a[1]*b[0],
  ])

proc cross*[ScalarA, ScalarB](a: Vector[7, ScalarA], b: Vector[7, ScalarB]): auto =
  result = initVector[7, productT(ScalarA, ScalarB)]([
     a[1]*b[2] - a[2]*b[1] - a[3]*b[4] + a[4]*b[3] - a[5]*b[6] + a[6]*b[5],
    -a[0]*b[2] + a[2]*b[0] - a[3]*b[5] + a[4]*b[6] + a[5]*b[3] - a[6]*b[4],
     a[0]*b[1] - a[1]*b[0] - a[3]*b[6] - a[4]*b[5] + a[5]*b[4] + a[6]*b[3],
     a[0]*b[4] + a[1]*b[5] + a[2]*b[6] - a[4]*b[0] - a[5]*b[1] - a[6]*b[2],
    -a[0]*b[3] - a[1]*b[6] + a[2]*b[5] + a[3]*b[0] - a[5]*b[2] + a[6]*b[1],
     a[0]*b[6] - a[1]*b[3] - a[2]*b[4] + a[3]*b[1] + a[4]*b[2] - a[6]*b[0],
    -a[0]*b[5] + a[1]*b[4] - a[2]*b[3] + a[3]*b[2] - a[4]*b[1] + a[5]*b[0],
  ])

func length2*[N: static int, T](v: Vector[N, T]): auto =
  result = default(typeof(v[0] * v[0]))
  for i in 0..<N:
    result += v[i] * v[i]

func length*[N: static int, T](v: Vector[N, T]): T =
  sqrt(v.length2())

func normalized*[N: static int, T](v: Vector[N, T]): auto =
  v / v.length()

func normalize*[N: static int, T](v: var Vector[N, T]) =
  v = v.normalized()

export
  matrices.`$`,
  matrices.`+`,
  matrices.`-`,
  matrices.`*`,
  matrices.`/`
 
{.pop.}
