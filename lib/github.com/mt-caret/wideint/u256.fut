import "wideint"

module u256: wideint = {
  module u32m = u32
  module u64m = u64
  module i32m = i32
  module i64m = i64

  type eight 'a = (a,a,a,a,a,a,a,a)
  type t = eight u32

  -- utilities

  let fill (x: u32): t = (x,x,x,x,x,x,x,x)
  let wrap (n: u64): t = (u32.u64 n , u32.u64 (n >> 32), 0, 0, 0, 0, 0, 0)

  let zero: t = fill 0
  let one: t = wrap 1

  let map' 'a 'b (f: a -> b) (t: eight a): eight b =
    (f t.1, f t.2, f t.3, f t.4, f t.5, f t.6, f t.7, f t.8)

  let unzip' 'a 'b (ab: eight (a, b)): (eight a, eight b) =
    (map' (.1) ab, map' (.2) ab)

  let map_to_array 'a (f: u32 -> a) (t: t): [8]a =
    [f t.1, f t.2, f t.3, f t.4, f t.5, f t.6, f t.7, f t.8]

  let map_of_array 'a (f: a -> u32) (xs: [8]a): t =
    (f xs[0], f xs[1], f xs[2], f xs[3], f xs[4], f xs[5], f xs[6], f xs[7])

  let map2' 'a 'b 'c (f: a -> b -> c) (t1: eight a) (t2: eight b): eight c =
    ( f t1.1 t2.1
    , f t1.2 t2.2
    , f t1.3 t2.3
    , f t1.4 t2.4
    , f t1.5 t2.5
    , f t1.6 t2.6
    , f t1.7 t2.7
    , f t1.8 t2.8
    )

  let get 't (index: i32) (x: eight t): t =
    if index == 0 then x.1 else
    if index == 1 then x.2 else
    if index == 2 then x.3 else
    if index == 3 then x.4 else
    if index == 4 then x.5 else
    if index == 5 then x.6 else
    if index == 6 then x.7 else
    x.8

  let set 't (index: i32) (xs: eight t) (value: t): eight t =
    assert (index >= 0 && index < 8)
      (map2' (\x i -> if i == index then value else x) xs  (0,1,2,3,4,5,6,7))

  let reduce' 'a (f: a -> a -> a) (t: eight a): a =
    f (f (f t.1 t.2) (f t.3 t.4)) (f (f t.5 t.6) (f t.7 t.8))

  let foldl' 'a 'b (f: b -> a -> b) (init: b) (t: eight a): b =
    (f (f (f (f (f (f (f (f init t.1) t.2) t.3) t.4) t.5) t.6) t.7) t.8)

  let foldr' 'a 'b (f: a -> b -> b) (init: b) (t: eight a): b =
    (f t.8 (f t.7 (f t.6 (f t.5 (f t.4 (f t.3 (f t.2 (f t.1 init))))))))

  -- contents

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

  let to_u64 (t: t): u64 = u64m.u32 t.1 | ((u64m.u32 t.2) << 32)
  let to_i64: t -> i64 = i64m.u64 <-< to_u64

  let highest: t = fill u32m.highest
  let lowest: t = zero

  let and: t -> t -> t = map2' (&)
  let or:  t -> t -> t = map2' (|)
  let xor: t -> t -> t = map2' (^)
  let not: t -> t = map' (~)

  let sum_u64 (a: [8]u64) (b: [8]u64): [8]u64 =
    loop ret = replicate 8 0 for i in 0..<8 do
      ret with [i] = a[i] + b[i]

  let shift_add (t: eight u64): t =
    let t1 = t.1
    let t2 = t.2 + (t1 >> 32)
    let t3 = t.3 + (t2 >> 32)
    let t4 = t.4 + (t3 >> 32)
    let t5 = t.5 + (t4 >> 32)
    let t6 = t.6 + (t5 >> 32)
    let t7 = t.7 + (t6 >> 32)
    let t8 = t.8 + (t7 >> 32)
    in
    ( t1, t2, t3, t4, t5, t6, t7, t8)
      |> map' u32m.u64

  let add (a: t) (b: t): t =
    map2' (+) (map' u64m.u32 a) (map' u64m.u32 b)
      |> shift_add

  let negate = add one <-< xor highest

  let sub (a: t) (b: t): t =
    add a (negate b)

  let shift_tuple_left (shift : i32) (t : t) : t =
    assert (shift >= 0)
      (if shift == 0 then t else
      if shift == 1 then (0, t.1, t.2, t.3, t.4, t.5, t.6, t.7) else
      if shift == 2 then (0,   0, t.1, t.2, t.3, t.4, t.5, t.6) else
      if shift == 3 then (0,   0,   0, t.1, t.2, t.3, t.4, t.5) else
      if shift == 4 then (0,   0,   0,   0, t.1, t.2, t.3, t.4) else
      if shift == 5 then (0,   0,   0,   0,   0, t.1, t.2, t.3) else
      if shift == 6 then (0,   0,   0,   0,   0,   0, t.1, t.2) else
      if shift == 7 then (0,   0,   0,   0,   0,   0,   0, t.1) else
      zero)

  let mult (a: t) (b: t): t =
    let a64 = map' u64m.u32 a
    let b64 = map' u64m.u32 b
    let ith_mult i =
      map' (* (get i b64)) a64
        |> shift_add
        |> shift_tuple_left i
    in
    map' ith_mult (0,1,2,3,4,5,6,7)
      |> reduce' add

    -- TODO: following should work, but does not
    --let sum_up i =
    --  loop accum = (0, 0) for j in 0...i do
    --    let add_amt = (get j a64) * (get (i - j) b64) in
    --    ( accum.1 + if u64m.highest - add_amt < accum.2 then 1 else 0
    --    , accum.2 + add_amt
    --    )
    --let (carry, result) =
    --  map' sum_up (0,1,2,3,4,5,6,7)
    --    |> unzip'
    --in
    --  shift_add result
    --    |> add (shift_tuple_left 1 carry)

  let lt (a: t) (b: t): bool =
    a.8 < b.8 ||
    (a.8 == b.8 && (a.7 < b.7 ||
    (a.7 == b.7 && (a.6 < b.6 ||
    (a.6 == b.6 && (a.5 < b.5 ||
    (a.5 == b.5 && (a.4 < b.4 ||
    (a.4 == b.4 && (a.3 < b.3 ||
    (a.3 == b.3 && (a.2 < b.2 ||
    (a.2 == b.2 && a.1 < b.1)))))))))))))

  let gt: t -> t -> bool = flip lt

  let eq (a: t) (b: t): bool =
    map2' (==) a b
      |> reduce' (&&)

  let le (a: t) (b: t): bool = lt a b || eq a b
  let ge: t -> t -> bool = flip le

  let neq (a: t) (b: t): bool = ! (eq a b)

  let shl (t: t) (shift: u32): t =
    let i = shift % 32
    let carry =
      if i == 0 then zero else
      map' (>> (32 - i)) t |> shift_tuple_left 1
    in
    map' (<< i) t
      |> map2' (|) carry
      |> shift_tuple_left (i32m.u32 shift / 32)

  let shift_tuple_right (shift : i32) (t : t) : t =
    assert (shift >= 0)
      (if shift == 0 then t else
      if shift == 1 then (t.2, t.3, t.4, t.5, t.6, t.7, t.8,   0) else
      if shift == 2 then (t.3, t.4, t.5, t.6, t.7, t.8,   0,   0) else
      if shift == 3 then (t.4, t.5, t.6, t.7, t.8,   0,   0,   0) else
      if shift == 4 then (t.5, t.6, t.7, t.8,   0,   0,   0,   0) else
      if shift == 5 then (t.6, t.7, t.8,   0,   0,   0,   0,   0) else
      if shift == 6 then (t.7, t.8,   0,   0,   0,   0,   0,   0) else
      if shift == 7 then (t.8,   0,   0,   0,   0,   0,   0,   0) else
      zero)

  let shr (t: t) (shift: u32): t =
    let i = shift % 32
    let carry = 
      if i == 0 then zero else
      map' (<< (32 - i)) t |> shift_tuple_right 1
    in
    map' (>> i) t
      |> map2' (|) carry
      |> shift_tuple_right (i32m.u32 shift / 32)

  let num_bits: i32 = 256

  let get_bit (index: i32) (t: t): i32 =
    get (index / 32) t
      |> u32m.get_bit (index % 32)


  let set_bit (index: i32) (t: t) (bit: i32): t =
    let i = index / 32
    let current = get i t
    let onehot = 1 << (u32m.i32 index % 32)
    let next =
      if bit == 0
      then current & (u32m.negate onehot)
      else current | onehot
    in
      set i t next


  let divmod (a: t) (b: t): (t, t) =
    loop (q, r) = (zero, zero) for i in 255..254...0 do
      let tr = add (shl r 1) (i32 (get_bit i a))
      let overflow = ge tr b
      in
      (add (shl q 1) (bool overflow), if overflow then sub tr b else tr)

  let div (a: t) (b: t): t =
    (divmod a b).1

  let mod (a: t) (b: t): t =
    (divmod a b).2

  -- exponentiation by squaring
  let pow (a: t) (b: t): t =
    (loop (product, value) = (one, a) for i in 0..<256 do
      ( if 1 == get_bit i b
        then mult product value
        else product
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

-- wideint-specific

  let abstract = map_of_array id
  let expose = map_to_array id

  let pack [n] (xs: [n]bool): t =
    loop ret = zero for i in 0..<n do
      if xs[i]
      then or ret (shl one (u32m.i32 i))
      else ret

  let unpack (t: t): [256]bool =
    map (\i -> u32m.get_bit (i % 32) (get (i / 32) t) == 1) (0..<256)

  let popcount_u32 (n: u32): i32 =
    loop ret = 0 for i in 0..<32 do
      ret + i32m.u32 ((n >> i) & 1)

  let popcount (t: t): i32 =
    map' popcount_u32 t
      |> reduce' (+)

-- aliasing

  let (+) = add
  let (-) = sub
  let (*) = mult
  let (/) = div
  let (**) = pow
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
    then zero
    else shl t amt.1
  let (>>) t amt =
    if amt >= i32 num_bits
    then copy zero
    else shr t amt.1
  let (>>>) = (>>)
}
