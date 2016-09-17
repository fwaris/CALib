///Generate a test function of aribtrary complexity
///Paper: "A Test Problem Generator for Non-Stationary Environments" by Morrison & De Jong
module DF1
let private sqr x = x * x

let df1_2d (rnd:System.Random) n (hbase,hrange) (rbase,rrange) =
    let hs = [|for i in 1..n -> hbase + rnd.NextDouble() * hrange|]
    let rs = [|for i in 1..n -> rbase + rnd.NextDouble() * rrange|]
    let xi = [|for i in 1..n -> -1.0 + 2.0 * rnd.NextDouble()|]
    let yi = [|for i in 1..n -> -1.0 + 2.0 * rnd.NextDouble()|]
    let nList = [for i in 0 .. n-1 -> i]
    let f x y =
        let maxF m i = max m (hs.[i] - rs.[i] * sqrt (sqr (x - xi.[i]) + sqr (y - yi.[i])))
        (System.Double.MinValue,nList) ||> List.fold maxF
    f
(*
let testDfi = df1_2d (System.Random()) 5 (3.,3.) (5.,5.)
let landscape = 
    [for x in -1.0 .. 0.1 .. 1.0 do
        for y in -1.0 .. 0.1 .. 1.0 do
            yield (x,y,testDfi x y)]
*)
