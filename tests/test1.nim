# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest
import geonimetry
import std/math

suite "linearmath":
  test "matrix":
    let m = initMatrix([
      [1, 2, 3],
      [4, 5, 6],
      [7, 8, 9]
    ])
    check m.rowAt(0) == initMatrix([[1, 2, 3]])
    check m.rowAt(1) == initMatrix([[4, 5, 6]])
    check m.rowAt(2) == initMatrix([[7, 8, 9]])
    check m.colAt(0) == initMatrix([[1, 4, 7]]).tr
    check m.colAt(1) == initMatrix([[2, 5, 8]]).tr
    check m.colAt(2) == initMatrix([[3, 6, 9]]).tr

  test "rotation":
    let q = quatd(vector3(1.0, 0, 0), PI)


  test "transforms":
    check rotation(vector3(1.0, 0, 0), PI) == rotationX(PI)
    check rotation(vector3(0.0, 1, 0), PI) == rotationY(PI)
    check rotation(vector3(0.0, 0, 1), PI) == rotationZ(PI)
  
  test "swizzling":
    check vector3f(1, 2, 3).zyx == vector3f(3, 2, 1)
    var aaa = vector3f(0, 0, 0)
    aaa.zyx = [3'f32, 2, 1]
    check aaa == vector3f(1, 2, 3)

  test "cross product":
    check vector3f(1, 0, 0).cross(vector3f(0, 1, 0)) == vector3f(0, 0, 1)
