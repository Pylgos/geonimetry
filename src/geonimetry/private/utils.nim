type RemoveUnit*[T] = typeof(default(T) / default(T))

proc to*[T, U: SomeNumber](value: T, to: typedesc[U]): U =
  U(value)

proc to*[T](value: T, to: typedesc[string]): string =
  $value
