import ./linearmath/vectors
import ./private/utils

type
  AABB*[N: static int, T] = object
    min*: Vector[N, T]
    size*: Vector[N, T]

  AABB2f* = AABB[2, float32]
  AABB2d* = AABB[2, float64]

  AABB3f* = AABB[3, float32]
  AABB3d* = AABB[3, float64]

proc aabb*[N, T](min: Vector[N, T], size: Vector[N, T]): AABB[N, T] =
  result.min = min
  result.size = size

proc fromMinMax*[N, T](_: AABB[N, T], min, max: Vector[N, T]): AABB[N, T] =
  result.min = min
  result.size = max - min

proc max*[N, T](bbox: AABB[N, T]): Vector[N, T] =
  bbox.min + bbox.size

proc center*[N, T](bbox: AABB[N, T]): Vector[N, T] =
  bbox.min + bbox.size * 0.5.to(RemoveUnit[T])

proc contains*[N, T](bbox: AABB[N, T], v: Vector[N, T]): bool =
  let min = bbox.min
  let max = bbox.max
  for i in 0..<N:
    if not (min[i] <= v[i] and v[i] <= max[i]):
      return false
  return true
