
// https://en.wikipedia.org/wiki/Permuted_congruential_generator#Example_code

[<Struct>]
type Pcg32 = | Pcg32 of uint64

let multiplier = 6364136223846793005UL
let increment = 1442695040888963407UL

let raw (Pcg32 state) : Pcg32 * uint32 =
    let pcg = Pcg32 (state * multiplier + increment)

    let rotr32 (x:uint32) (r:int32) = x >>> r ||| x <<< (-r &&& 31)
    let value = rotr32 (uint32 ((state ^^^ (state >>> 18)) >>> 27)) (int32 (state >>> 59)) // 27 = 32 - 5
    (pcg, value)

let range (lower : int32, upper : int32) (pcg) : Pcg32 * int =
    let (pcg, raw) = raw pcg

    let size = uint64 <| abs (upper - lower)
    let offset = min lower upper
    (pcg, offset + int32 (uint64 raw * size / uint64 System.UInt32.MaxValue))

let make (seed:uint64) : Pcg32 =
    Pcg32 (seed + increment)
    |> raw
    |> fst
