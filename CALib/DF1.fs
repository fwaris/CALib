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
open System.IO
type Cone = {H:float; R:float; X:float; Y:float}

let maxF x y m c = 
    let dist = sqrt (sqr (x - c.X) + sqr (y - c.Y))
    max m (c.H - c.R * dist)

let createDf1 file =
    let lines = File.ReadLines file |> Seq.skip 5
    let cones =
        lines
        |> Seq.filter (fun s -> s.Trim().Length > 10)
//        |> Seq.mapi(fun i s -> printfn "%d - %s" i s; s)
        |> Seq.map(fun s -> s.Split([|','; ' '; '\t'|],System.StringSplitOptions.RemoveEmptyEntries))
        |> Seq.map(fun xs -> xs |> Array.map float)
        |> Seq.map (fun xs -> {X=xs.[0]; Y=xs.[1]; H=xs.[2]; R=xs.[3]})
        |> Seq.toArray
    let maxCone = cones |> Array.maxBy (fun x -> x.H)
    let df x y = (System.Double.MinValue,cones) ||> Array.fold (maxF x y)
    maxCone,df
(*
let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../Landscapes/test_cone3.5.csv")
let df1 = createDf1 @"C:\Users\cz8gb9\Documents\Visual Studio 2015\Projects\CALib\CALib\Landscapes\test_cone1.01.csv"
[for i in -1. .. 0.1 .. 1. do
    for j in -1. .. 0.1 .. 1. do
        yield df1 i j]
*)
        

    