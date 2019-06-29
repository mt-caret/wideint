import "u256"
open u256

entry add  (a: [8]u32) (b: [8]u32): [8]u32 = expose ((abstract a) + (abstract b))
entry sub  (a: [8]u32) (b: [8]u32): [8]u32 = expose ((abstract a) - (abstract b))
entry mult (a: [8]u32) (b: [8]u32): [8]u32 = expose ((abstract a) * (abstract b))
entry div  (a: [8]u32) (b: [8]u32): [8]u32 = expose ((abstract a) / (abstract b))

entry pack_unpack_roundtrip: [8]u32 -> [8]u32 = expose <-< pack <-< unpack <-< abstract

entry popcount: [8]u32 -> i32 = popcount <-< abstract
