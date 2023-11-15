import std/[macros, strutils, math, rationals]
import ../private/utils
import toconv

{.push inline.}

type
  Matrix*[N, M: static int; T = float] = object
    data*: array[N, array[M, T]]
  
  Matrix2x2f* = Matrix[2, 2, float32]
  Matrix2x2d* = Matrix[2, 2, float64]
  Matrix3x3f* = Matrix[3, 3, float32]
  Matrix3x3d* = Matrix[3, 3, float64]

template rows*(m: Matrix): int =
  m.N

template cols*(m: Matrix): int =
  m.M

template size*(m: Matrix): int =
  m.data.len

template elementType*(m: Matrix): typedesc =
  typeof(m.data[0][0])

template `[]`*[N, M: static int; T](m: Matrix[N, M, T], row, col: int): T =
  m.data[row][col]

template `[]`*[N, M: static int; T](m: var Matrix[N, M, T], row, col: int): var T =
  m.data[row][col]

template `[]=`*[N, M: static int; T](m: var Matrix[N, M, T], row, col: int, val: T) =
  m.data[row][col] = val

template considerBackwardIndex(len: int, i: int | BackwardsIndex): int =
  when i is BackwardsIndex:
    len - i.int
  else:
    i

proc initMatrix*[N, M: static int, T](data: sink array[N, array[M, T]]): Matrix[N, M, T] =
  result.data = data

template toNormalSlice[T, U: int | BackwardsIndex](len: int, sl: HSlice[T, U]): Slice[int] =
  considerBackwardIndex(len, sl.a) .. considerBackwardIndex(len, sl.b)

proc slice*[N, M: static int; T](m: Matrix[N, M, T], row, col: int, rowWidth, colWidth: static int): Matrix[rowWidth, colWidth, T] =
  result = Matrix[rowWidth, colWidth, T]()
  for r in 0..<rowWidth:
    for c in 0..<colWidth:
      result[r, c] = m[r + row, c + col]

proc rowAt*[N, M: static int, T](m: Matrix[N, M, T], i: int): Matrix[1, M, T] =
  m.slice(i, 0, 1, M)

proc colAt*[N, M: static int, T](m: Matrix[N, M, T], i: int): Matrix[N, 1, T] =
  m.slice(0, i, N, 1)

proc `[]`*[N, M: static int; T; T1, U1, T2, U2: int | Backwardsindex](m: Matrix[N, M, T], rowSlice: static HSlice[T1, U1], colSlice: static HSlice[T2, U2]): auto =
  const rSlice = toNormalSlice(m.rows, rowSlice)
  const cSlice = toNormalSlice(m.cols, colSlice)
  result = Matrix[rSlice.len, cSlice.len, T]()
  for row in 0..<result.rows:
    for col in 0..<result.cols:
      result[row, col] = m[rSlice.a + row, cSlice.a + col]

proc `[]`*[N, M: static int; T; T1, U1: int | Backwardsindex](m: Matrix[N, M, T], rowSlice: static HSlice[T1, U1], colIdx: static int): auto =
  m[rowSlice, colIdx..colIdx]

proc `[]`*[N, M: static int; T; T1, U1: int | Backwardsindex](m: Matrix[N, M, T], rowIdx: static int, colSlice: static HSlice[T1, U1]): auto =
  m[rowIdx..rowIdx, colSlice]

proc `$`*(m: Matrix): string =
  if m.rows == 0 and m.cols == 0: return "[]"
  elif m.rows == 1:
    result.add "["
    for col in 0..<m.cols:
      result.add $m[0, col]
      if col != m.cols - 1:
        result.add ", "
    result.add "]"
  elif m.cols == 1:
    result.add "["
    for row in 0..<m.rows:
      result.add $m[row, 0]
      if row != m.rows - 1:
        result.add ", "
    result.add "]ᵀ"
  else:
    var colWidth: array[m.M, int]
    var strings: array[m.N, array[m.M, string]]

    for row in 0..<m.rows:
      for col in 0..<m.cols:
        strings[row][col] = $m[row, col]
        colWidth[col] = max(colWidth[col], strings[row][col].len)

    for row in 0..<m.rows:
      if row == 0:
        result.add "⎡"
      elif row == m.rows - 1:
        result.add "⎣"
      else:
        result.add "⎢"
      
      for col in 0..<m.cols:
        let s = strings[row][col]
        result.add s.align(colWidth[col])
        if col == m.cols - 1:
          result.add ""
        else:
          result.add " "

      if row == 0:
        result.add "⎤\n"
      elif row == m.rows - 1:
        result.add "⎦"
      else:
        result.add "⎥\n"

proc `+`*[N, M: static int, T](a, b: Matrix[N, M, T]): Matrix[N, M, T] =
  for row in 0..<N:
    for col in 0..<M:
      result[row, col] = a[row, col] + b[row, col]

proc `-`*[N, M: static int, T](a, b: Matrix[N, M, T]): Matrix[N, M, T] =
  for row in 0..<N:
    for col in 0..<M:
      result[row, col] = a[row, col] - b[row, col]

proc `+`*[N, M: static int, T](m: Matrix[N, M, T]): Matrix[N, M, T] =
  m

proc `-`*[N, M: static int, T](m: Matrix[N, M, T]): Matrix[N, M, T] =
  for row in 0..<N:
    for col in 0..<M:
      result[row, col] = -m[row, col]

proc `*`*[N, M: static int, T, U](m: Matrix[N, M, T], scalar: U): auto =
  result = Matrix[N, M, typeof(default(T) * default(U))]()
  for row in 0..<N:
    for col in 0..<M:
      result[row, col] = m[row, col] * scalar

proc `*=`*[T](m: var Matrix, scalar: T) =
  m = m * scalar

proc `/`*[N, M: static int, T, U](m: Matrix[N, M, T], divider: U): auto =
  result = Matrix[N, M, typeof(default(T) / default(U))]()
  for row in 0..<N:
    for col in 0..<M:
      result[row, col] = m[row, col] / divider

proc `/=`*[T](m: var Matrix, divider: T) =
  m = m / divider

proc dot*[N, M, O: static int; T, U](a: Matrix[N, M, T], b: Matrix[M, O, U]): auto =
  result = Matrix[N, O, typeof(default(T) * default(U))]()
  for i in 0..<N:
    for j in 0..<O:
      for k in 0..<M:
        result[i, j] += a[i, k] * b[k, j]

template `*`*[N, M, O: static int; T, U](a: Matrix[N, M, T], b: Matrix[M, O, U]): auto =
  dot(a, b)

proc det*[N: static int, T](m: Matrix[N, N, T]): auto =
  when N == 0:
    result = 1.to(RemoveUnit[T])
  elif N == 1:
    result = m[0,0]
  elif N == 2:
    result = m[0,0]*m[1,1] - m[0,1]*m[1,0]
  elif N == 3:
    result =  m[0,0]*m[1,1]*m[2,2]
    result += m[1,0]*m[2,1]*m[0,2]
    result += m[2,0]*m[0,1]*m[1,2]
    result -= m[2,0]*m[1,1]*m[0,2]
    result -= m[1,0]*m[0,1]*m[2,2]
    result -= m[0,0]*m[2,1]*m[1,2]
  else:
    let zero = 0.to(T)
    const TisSomeUnit = typeof(zero*zero) isnot typeof(zero)
    when TisSomeUnit:
      type ResultT = typeof(zero^N)
    else:
      type ResultT = T
    var tmp = m
    for i in 0..<N:  
      if tmp[i,i] != zero:
        continue  
      var k = 0
      while k < N: 
        if tmp[k,i] != zero:
          break
        inc k
      if k == N:
        return 0.to(ResultT)
      for j in 0..<N:
        tmp[i,j] += tmp[k,j]
    for i in 0..<N:
      for j in 0..<N:  
        if i == j:
          continue
        let c = tmp[j,i] / tmp[i,i];  
        for k in 0..<N: 
          tmp[j,k] -= c * tmp[i,k]
    var det = m[0,0].to(RemoveUnit[T])
    for i in 1..<N:
      det *= tmp[i,i].to(RemoveUnit[T])
    result = det.to(ResultT)

proc tr*[N, M: static int, T](m: Matrix[N, M, T]): Matrix[M, N, T] =
  for i in 0..<N:
    for j in 0..<M:
      result[j, i] = m[i, j]

proc orthonormalized*[T](m: Matrix[3, 3, T]): auto =
  var
    x = m[0, 0..^1].tr
    y = m[1, 0..^1].tr
    z = m[2, 0..^1].tr
  let xn = x.normalized()
  y = y - xn * xn.tr.dot(y)
  let yn = y.normalized()
  z = (z - xn * xn.tr.dot(z)) - yn * yn.tr.dot(z)
  let zn = z.normalized()
  initMatrix[3, 3, typeof(xn[0])]([
    [xn.x, xn.y, xn.z],
    [yn.x, yn.y, yn.z],
    [zn.x, zn.y, zn.z],
  ])

proc orthonormalize*(m: var Matrix) =
  m = orthonormalized(m)

proc identity*[N: static int, T](_: typedesc[Matrix[N, N, T]]): Matrix[N, N, T] =
  for i in 0..<N:
    result[i, i] = 1.to(T)




{.pop.}
