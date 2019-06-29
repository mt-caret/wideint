module type u256t = {
  include size
  val abstract: [8]u32 -> t
  val expose: t -> [8]u32

  val pack: []bool -> t
  val unpack: t -> [256]bool

  val popcount: t -> i32
}

module u256: u256t = {
  module u32m = u32
  module u64m = u64
  module i32m = i32
  module i64m = i64

  type t = [8]u32

  let wrap (n: u64): t =
    [u32.u64 n , u32.u64 (n >> 32), 0, 0, 0, 0, 0, 0]

  let zero: t = replicate 8 0
  let one: t = wrap 1

  let i8   = wrap <-< u64m.i8
  let i16  = wrap <-< u64m.i16
  let i32  = wrap <-< u64m.i32
  let i64  = wrap <-< u64m.i64

  let u8   = wrap <-< u64m.u8
  let u16  = wrap <-< u64m.u16
  let u32  = wrap <-< u64m.u32
  let u64  = wrap <-< u64m.u64

  let f32  = wrap <-< u64m.f32
  let f64  = wrap <-< u64m.f64

  let bool = wrap <-< u64m.bool

  let to_u64 (t: t): u64 = u64m.u32 t[0] | ((u64m.u32 t[1]) << 32)
  let to_i64: t -> i64 = i64m.u64 <-< to_u64

  let highest: t = replicate 8 u32m.highest
  let lowest: t = zero

  let and: t -> t -> t = map2 (&)
  let or:  t -> t -> t = map2 (|)
  let xor: t -> t -> t = map2 (^)
  let not: t -> t = map (~)

  let add (a: t) (b: t): t =
    let sum64 = map2 (+) (map u64m.u32 a) (map u64m.u32 b) in
    (loop sum64 for i in 0..<7 do
      sum64 with [i+1] = sum64[i+1] + (sum64[i] >> 32))
      |> map u32m.u64

  let negate = add (copy one) <-< xor highest

  let sub (a: t) (b: t): t =
    add a (negate b)

  -- TODO: this is naive, prob can make faster
  let mult (a: t) (b: t): t =
    let a64 = map u64m.u32 a
    let b64 = map u64m.u32 b
    -- TODO: possibly use reduce? (probably invalid)
    let sum_up i =
      loop sum = 0 for j in 0...i do
        sum + a64[j] * b64[i - j]
    let c64 = map sum_up (0..<8)
    in
    (loop c64 for i in 0..<7 do
      c64 with [i + 1] = c64[i + 1] + (c64[i] >> 32))
      |> map u32m.u64

  let lt (a: t) (b: t): bool =
    (loop (i, _) = (7,  a[7] < b[7])
    while i > 0 && a[i] == b[i]
    do (i - 1, a[i - 1] < b[i - 1])).2

  let gt: t -> t -> bool = flip lt

  let eq (a: t) (b: t): bool =
    map2 (==) a b
      |> all id

  let le (a: t) (b: t): bool = lt a b || eq a b
  let ge: t -> t -> bool = flip le

  let neq (a: t) (b: t): bool = ! (eq a b)

  let shift_array_left (shift : i32) (n : t) : t =
    if shift > 7
    then copy zero
    else concat (replicate shift 0) n[0:(8-shift)]

  let shl (t: t) (shift: u32): t =
    let i = shift % 32
    let carry =
      if i == 0
      then replicate 8 0
      else map (>> (32 - i)) t |> shift_array_left 1
    in
    map (<< i) t
      |> map2 (|) carry
      |> shift_array_left (i32m.u32 shift / 32)

  let shift_array_right (shift : i32) (n : t) : t =
    if shift > 7
    then copy zero
    else concat n[shift:8] (replicate shift 0)

  let shr (t: t) (shift: u32): t =
    let i = shift % 32
    let carry = 
      if i == 0
      then replicate 8 0
      else map (<< (32 - i)) t |> shift_array_right 1
    in
    map (>> i) t
      |> map2 (|) carry
      |> shift_array_right (i32m.u32 shift / 32)

  let num_bits: i32 = 256

  let get_bit (index: i32) (t: t): i32 =
    (i32m.u32 (shr t (u32m.i32 index))[0]) & 1

  let set_bit (index: i32) (t: t) (bit: i32): t =
    let i = index / 32
    let onehot = 1 << (u32m.i32 index % 32)
    in
    copy t with [i] =
      if bit == 0
      then t[i] & (u32m.negate onehot)
      else t[i] | onehot

  let pack [n] (xs: [n]bool): t =
    loop ret = copy zero for i in 0..<n do
      if xs[i]
      then or ret (shl (copy one) (u32m.i32 i))
      else ret

  let unpack (t: t): [256]bool =
    map (\i -> u32m.get_bit (i % 32) (unsafe t[i / 32]) == 1) (0..<256)

  -- TODO: is a parallel implementation better here?
  let popcount (t: t): i32 =
    loop ret = 0 for i in 0..<256 do
      ret + get_bit i t

  let divmod (a: t) (b: t): (t, t) =
    loop (q, r) = (copy zero, copy zero) for i in 255..254...0 do
      let tr = add (shl r 1) (i32 (get_bit i a))
      let overflow = ge tr b
      in
      (add (shl q 1) (bool overflow), if overflow then sub tr b else tr)

  let div (a: t) (b: t): t =
    (divmod a b).1

  let mod (a: t) (b: t): t =
    (divmod a b).2

  -- exponentiation by squaring
  -- TODO: is it possible to parallelize this?
  let exp (a: t) (b: t): t =
    (loop (sum, value) = (copy zero, a) for i in 0..<256 do
      ( if 1 == get_bit i b
        then add sum value
        else sum
      , mult value value
      )).1

  let max (a: t) (b: t): t = if gt a b then a else b
  let min (a: t) (b: t): t = if lt a b then a else b

  let abs: t -> t = id
  let sgn (a: t): t = if eq a (copy zero) then (copy zero) else (copy one)

  let sum: []t -> t = reduce add lowest
  let product: []t -> t = reduce mult one

  let maximum: []t -> t = reduce max lowest
  let minimum: []t -> t = reduce min highest

  let iota (t: t): *[]t =
    to_u64 t
      |> u64m.iota
      |> map u64
  let replicate 'v (t: t) (x: v): *[]v =
    u64m.replicate (to_u64 t) x

  let (+) = add
  let (-) = sub
  let (*) = mult
  let (/) = div
  let (**) = exp
  let (==) = eq
  let (<) = lt
  let (>) = gt
  let (<=) = le
  let (>=) = ge
  let (!=) = neq

  let (%) = mod
  let (//) = div
  let (%%) = mod
  let (&) = and
  let (|) = or
  let (^) = xor
  let (~) = not
  let (<<) t amt =
    if amt >= i32 num_bits
    then copy zero
    else shl t amt[0]
  let (>>) t amt =
    if amt >= i32 num_bits
    then copy zero
    else shr t amt[0]
  let (>>>) = (>>)

  let abstract = id
  let expose = id
}
