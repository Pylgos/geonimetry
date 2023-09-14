import std/math
import ../private/utils
import ./[matrices, vectors]
import toconv

{.push inline.}

type
  Quat*[T] = object
    x*, y*, z*, w*: T
  Quatf* = Quat[float32]
  Quatd* = Quat[float64]

func quat*[T](x, y, z, w: T): Quat[T] =
  Quat[T](x: x, y: y, z: z, w: w)

func quat*[T](axis: Vector[3, T], angle: T): Quat[T] =
  type Base = RemoveUnit[T]
  let d = axis.length()
  let s = sin(angle * 0.5'f32.to(Base)) / d
  quat[T](axis.x * s, axis.y * s, axis.z * s, cos(angle * 0.5.to(Base)))

func quatf*(x = 0'f32, y = 0'f32, z = 0'f32, w = 1'f32): Quatf =
  Quatf(x: x, y: y, z: z, w: w)

func quatf*(axis: Vector[3, float32], angle: float32): Quat[float32] =
  quat(axis, angle)

func quatd*(axis: Vector[3, float64], angle: float64): Quat[float64] =
  quat(axis, angle)

func quatd*(x = 0'f64, y = 0'f64, z = 0'f64, w = 1'f64): Quatd =
  Quatd(x: x, y: y, z: z, w: w)

func `*=`*[T, U](self: var Quat[T], rhs: U) =
  self.x *= rhs
  self.y *= rhs
  self.z *= rhs
  self.w *= rhs

func `-`[T](q: Quat[T]): Quat[T] =
  quat(-q.x, -q.y, -q.z, -q.w)

func `*`*[T, U](self: Quat[T], scalar: U): auto =
  quat(self.x * scalar, self.y * scalar, self.z * scalar, self.w * scalar)

func `/`*[T, U](self: Quat[T], divider: U): auto =
  quat(self.x / divider, self.y / divider, self.z / divider, self.z / divider)

func dot*[T, U](a: Quat[T], b: Quat[U]): auto =
  a.x * b.x + a.y * b.y + a.z * b.z + a.w * b.w

func `*`*(a, b: Quat): Quat =
  quat(
    a.w * b.x + a.x * b.w + a.y * b.z - a.z * b.y,
    a.w * b.y + a.y * b.w + a.z * b.x - a.x * b.z,
    a.w * b.z + a.z * b.w + a.x * b.y - a.y * b.x,
    a.w * b.w - a.x * b.x - a.y * b.y - a.z * b.z,
  )

func length2*(a: Quat): auto =
  a.dot(a)

func length*(a: Quat): auto =
  mixin sqrt
  sqrt(a.length2())

func normalize*(q: var Quat) =
  q = q / q.length()

func normalized*(q: Quat): Quat =
  q / q.length()

func inverse*(a: Quat): Quat =
  quat(-a.x, -a.y, -a.z, w)

func angle*[T](q: Quat[T]): T =
  2 * arccos(q.w)

func axis*[T](q: Quat[T]): Vector[3, T] =
  mixin sqrt
  let sSquared = 1.to(T) - q.w * q.w
  if sSquared == 0.to(T):
    initVector([1.to(T), 0.to(T), 0.to(T)])
  else:
    let s = sqrt(sSquared)
    initVector([q.x * s, q.y * s, q.z * s])

func angleShortestPath*[T](a, b: Quat[T]): T =
  mixin sqrt
  let s = sqrt(a.length2 * b.length2)
  if a.dot(b) < 0:
    arccos(a.dot(-b) / s) * 2.to(RemoveUnit[T])
  else:
    arccos(a.dot(b) / s) * 2.to(RemoveUnit[T])

func xform*[T, U](q: Quat[T], v: Vector[3, U]): auto =
  let u = initVector([q.x, q.y, q.z])
  let uv = u.cross(v)
  return v + ((uv * q.w) + u.cross(uv) * 2'f)

func fromRPY*[T](_: typedesc[Quat[T]], roll, pitch, yaw: T): Quat[T] =
  let
    halfYaw = yaw * 0.5
    halfPitch = pitch * 0.5
    halfRoll = roll * 0.5
    cosYaw = cos(halfYaw)
    sinYaw = sin(halfYaw)
    cosPitch = cos(halfPitch)
    sinPitch = sin(halfPitch)
    cosRoll = cos(halfRoll)
    sinRoll = sin(halfRoll)
  quat(
    sinRoll * cosPitch * cosYaw - cosRoll * sinPitch * sinYaw,
    cosRoll * sinPitch * cosYaw + sinRoll * cosPitch * sinYaw,
    cosRoll * cosPitch * sinYaw - sinRoll * sinPitch * cosYaw,
    cosRoll * cosPitch * cosYaw + sinRoll * sinPitch * sinYaw,
  )

func toBasis*[T](q: Quat[T]): Matrix[3, 3, T] =
  let d = q.length2()
  let s = 2.to(RemoveUnit[T]) / d
  let
    xs = q.x * s
    ys = q.y * s
    zs = q.z * s
    
    wx = q.w * xs
    wy = q.w * ys
    wz = q.w * zs
    
    xx = q.x * xs
    xy = q.x * ys
    xz = q.x * zs

    yy = q.y * ys
    yz = q.y * zs
    zz = q.z * zs

  initMatrix[3, 3, T]([
    [1.to(RemoveUnit[T]) - (yy + zz), xy - wz, xz + wy],
    [xy + wz, 1.to(RemoveUnit[T]) - (xx + zz), yz - wx],
    [xz - wy, yz + wx, 1.to(RemoveUnit[T]) - (xx + yy)]
  ])

func fromQuat*[T](_: typedesc[Matrix[3, 3, T]], q: Quat[T]): Matrix[3, 3, T] =
  q.toBasis()

func toQuat*[T](m: Matrix[3, 3, T]): Quat[T] =
  var m = m.orthonormalized()
  let det = m.det()
  if det < 0.to(typeof(det)):
    m *= -1.to(RemoveUnit[T])
  let trace = m[0, 0] + m[1, 1] + m[2, 2]
  var tmp: array[4, T]
  if trace > 0.to(T):
    var s = sqrt(trace + 1.to(RemoveUnit[T]))
    tmp[3] = s * 0.5.to(RemoveUnit[T])
    s = 0.5.to(RemoveUnit[T]) / s
    tmp[0] = m[2, 1] - m[1, 2] * s
    tmp[1] = m[0, 2] - m[2, 0] * s
    tmp[2] = m[1, 0] - m[0, 1] * s
  else:
    let i =
      if m[0, 0] < m[1, 1]:
        if m[1, 1] < m[2, 2]:
          2
        else:
          1
      else:
        if m[0, 0] < m[2, 2]:
          2
        else:
          0
    let j = (i + 1) mod 3
    let k = (i + 2) mod 3
    var s = sqrt(m[i, i] - m[j, j] - m[k, k] + T(1))
    tmp[i] = s * 0.5.to(RemoveUnit[T])
    s = 0.5.to(RemoveUnit[T]) / s
    tmp[3] = m[k, j] - m[j, k] * s
    tmp[j] = m[j, i] + m[i, j] * s
    tmp[k] = m[k, i] + m[i, k] * s
  Quat[T](x: tmp[0], y: tmp[1], z: tmp[2], w: tmp[3])

func fromBasis*[T](_: typedesc[Quat[T]], b: Matrix[3, 3, T]): Quat[T] =
  b.toQuat()

{.pop.}
