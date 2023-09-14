import ./[matrices, quaternions, vectors]
import ../private/utils
import std/[macros, math, typetraits]

{.push inline.}

proc rotationX*[T](angle: T): auto =
  let
    c = cos(angle)
    s = sin(angle)
  type M = typeof(c)
  initMatrix([
    [1.to(M), 0.to(M), 0.to(M)],
    [0.to(M), c, -s],
    [0.to(M), s, c]
  ])

proc rotationY*[T](angle: T): auto =
  let
    c = cos(angle)
    s = sin(angle)
  type M = typeof(c)
  initMatrix([
    [c, 0.to(M), s],
    [0.to(M), 1.to(M), 0.to(M)],
    [-s, 0.to(M), c]
  ])

proc rotationZ*[T](angle: T): auto =
  let
    c = cos(angle)
    s = sin(angle)
  type M = typeof(c)
  initMatrix([
    [c, -s, 0.to(M)],
    [s, c, 0.to(M)],
    [0.to(M), 0.to(M), 1.to(M)]
  ])

proc rotation*[T, U](axis: Vector[3, T], angle: U): auto =
  let
    n = axis.normalized()
    c = cos(angle)
    s = sin(angle)
    t = 1.to(typeof(c)) - c
    sq = vector3(n.x * n.x, n.y * n.y, n.z * n.z)
    xy = n.x*n.y
    yz = n.y*n.z
    zx = n.z*n.x
  initMatrix([
    [c + sq.x*t, xy*t-n.z*s, zx*t+n.y*s],
    [xy*t+n.z*s, c + sq.y*t, yz*t-n.x*s],
    [zx*t-n.y*s, yz*t+n.x*s, c + sq.z*t]
  ])

type
  Transform*[N: static int, T] = object
    basis*: Matrix[N, N, RemoveUnit[T]]
    origin*: Vector[N, T]
  
  Transform2f* = Transform[2, float32]
  Transform2d* = Transform[2, float64]
  Transform3f* = Transform[3, float32]
  Transform3d* = Transform[3, float64]

template Basis*[N: static int, T](_: typedesc[Transform[N, T]]): untyped = Matrix[N, N, default(T) / default(T)]

proc transform*[N: static int, T, U](origin: Vector[N, T], basis: Matrix[N, N, U]): Transform[N, T] =
  Transform[N, T](origin: origin, basis: basis)

proc transform3*[T](): Transform[3, T] =
  transform(vector3[T](), Transform[3, T].Basis.identity())

proc transform3*[T, U](origin: Vector[3, T], basis: Matrix[3, 3, U]): Transform[3, T] =
  transform(origin, basis)

proc transform3*[T, U](origin: Vector[3, T], rotation: Quat[U]): Transform[3, T] =
  transform(origin, rotation.toBasis())

proc transform2*[T](): Transform[2, T] =
  transform(vector3[T](), Transform[2, T].Basis.identity())

proc transform2*[T, U](origin: Vector[2, T], basis: Matrix[2, 2, U]): Transform[3, T] =
  transform(origin, basis)

proc identity*[N, T](_: typedesc[Transform[N, T]]): Transform[N, T] =
  transform(Vector[N, T](), Transform[N, T].Basis.identity())

proc rotation*[T](tf: Transform[3, T]): auto =
  tf.basis.toQuat()

proc `rotation=`*[T](tf: var Transform[3, T], q: Quat) =
  tf.basis = q.toBasis()

proc inverse*(tf: Transform): auto =
  let inv = tf.basis.tr()
  transform3(inv * -tf.origin, inv)

proc xform*[N, T, U](tf: Transform[N, T], v: Vector[N, U]): auto =
  tf.basis * v + tf.origin

proc xform*[T, U](tf: Transform[3, T], q: Quat[U]): auto =
  tf.rotation * q

proc `*`*[N, T](a, b: Transform[N, T]): auto =
  transform3(a.xform(b.origin), a.basis * b.basis)

proc rotated*[N, T, U](tf: Transform[N, T], b: Matrix[N, N, U]): Transform[N, T] =
  transform3(tf.origin, b * tf.basis)

proc rotated*[N, T, U](tf: Transform[N, T], q: Quat[U]): Transform[N, T] =
  transform3(tf.origin, q.toBasis() * tf.basis)

proc rotated*[T, U, V](tf: Transform[3, T], axis: Vector[3, U], angle: V): Transform[3, T] =
  tf.rotated(rotation(axis, angle))

proc `$`*[N, T](tf: Transform[N, T]): string =
  proc label(i: int): string =
    when N <= 3:
      ["X", "Y", "Z"][i]
    else:
      $i
  result = "("
  for colIdx in 0..<N:
    result &= label(colIdx)
    result &= ": "
    result &= $tf.basis.colAt(colIdx)
    result &= ", "
  result &= "O: "
  result &= $tf.origin
  result &= ")"

proc getRPY*[T](m: Matrix[3, 3, T]): Vector[3, T] =
  const epsilon = 0.0001
  let sy = m[2, 0]
  if sy < (1 - epsilon).to(T):
    if sy > -(1 - epsilon).to(T):
      result.x = arctan2(m[2, 1], m[2, 2])
      result.y = arcsin(-sy)
      result.z = arctan2(m[1, 0], m[0, 0])
    else:
      result.x = 0.to(T)
      result.y = (PI / 2).to(T)
      result.z = -arctan2(m[0, 1], m[1, 1])
  else:
    result.x = 0.to(T)
    result.y = (-PI / 2).to(T)
    result.z = -arctan2(m[0, 1], m[1, 1])

{.pop.}
