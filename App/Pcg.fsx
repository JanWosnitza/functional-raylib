
// https://en.wikipedia.org/wiki/Permuted_congruential_generator#Example_code

[<Struct>]
type Pcg = | Pcg of uint64

let multiplier = 6364136223846793005UL
let increment = 1442695040888963407UL

let raw (Pcg state) : Pcg * uint32 =
    let pcg = Pcg (state * multiplier + increment)

    let rotr32 (x:uint32) (r:int32) = x >>> r ||| x <<< (-r &&& 31)
    let value = rotr32 (uint32 ((state ^^^ (state >>> 18)) >>> 27)) (int32 (state >>> 59)) // 27 = 32 - 5
    (pcg, value)

let range (lower : int32, upper : int32) (pcg) : Pcg * int =
    let (pcg, raw) = raw pcg

    let size = uint64 <| abs (upper - lower)
    let offset = min lower upper
    (pcg, offset + int32 (uint64 raw * size / uint64 System.UInt32.MaxValue))

let make (seed:uint64) : Pcg =
    Pcg (seed + increment)
    |> raw
    |> fst

[<Struct>]
type PcgM<'a> = PcgM' of (Pcg -> Pcg * 'a)

module PcgM =
    let unit (x) = PcgM' (fun pcg -> (pcg, x))

    let bind (binder) (PcgM' m) = PcgM' <| fun pcg ->
        let (pcg, x) = m pcg
        let (PcgM' f') = binder x
        f' pcg

    let map (mapper) (PcgM' m) = PcgM' <| fun pcg ->
        let (pcg, x) = m pcg
        (pcg, mapper x)

    let getPcg = PcgM' (fun pcg -> pcg, pcg)

    let setPcg (pcg) = PcgM' (fun _ -> pcg, ())

    let left (PcgM' left) (PcgM' right) = PcgM' <| fun pcg ->
        let (pcg, x) = left pcg
        let (pcg, _) = right pcg
        (pcg, x)

    let right (PcgM' left) (PcgM' right) = PcgM' <| fun pcg ->
        let (pcg, _) = left pcg
        right pcg

    let raw  = PcgM' (fun pcg -> raw pcg)
    let range (lower, upper) = PcgM' (fun pcg -> range (lower, upper) pcg)

    let run (PcgM' m) (pcg) = m pcg

type PcgMBuilder() =
    member this.Bind(m, binder) = PcgM.bind binder m

    member this.Delay(f) : PcgM<_> = f ()

    member this.Run(m:PcgM<_>) = m

    member this.For(items, binder) =
        (PcgM.unit (), items)
        ||> Seq.fold (fun m item -> PcgM.right m (binder item))

    member this.Zero() = PcgM.unit ()

    member this.Return(x) = PcgM.unit x

    member this.ReturnFrom(m:PcgM<_>) = m

let pcgM = PcgMBuilder()

[<Struct>]
type PcgV<'a> =
    {
        Pcg : Pcg
        Value : 'a
    }

module PcgV =
    let unit (pcg) (value) = {Pcg = pcg; Value = value}

    let split (splitter) (pcgV:PcgV<_>) =
        let (left, right) = splitter pcgV.Value
        let pcgV =
            {
                Value = left
                Pcg = pcgV.Pcg
            }
        (pcgV, right)

    let apply (applier:_ -> PcgM<_>) (pcgV:PcgV<_>) =
        let (pcg, value) = PcgM.run (applier pcgV.Value) pcgV.Pcg
        {
            Pcg = pcg
            Value = value
        }

    let map (mapper:'a -> 'b) (pcgV:PcgV<'a>) : PcgV<'b> =
        {
            Pcg = pcgV.Pcg
            Value = mapper pcgV.Value
        }

    let make (applier) (seed) = unit (make seed) () |> apply applier
