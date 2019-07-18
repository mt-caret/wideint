module type wideint = {
  include size
  val abstract: []u32 -> t
  val expose: t -> []u32

  val pack: []bool -> t
  val unpack: t -> []bool

  val popcount: t -> i32
}


