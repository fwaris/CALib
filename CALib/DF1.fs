///Generate a test function of aribtrary complexity
///Paper: "A Test Problem Generator for Non-Stationary Environments" by Morrison & De Jong
module DF1
open Probability
open System.Collections.Generic

let private sqr x = x * x
type Cone = {H:float; R:float; L:float[]}

let genCones n dims (hbase,hrange) (rbase,rrange) = 
    let rnd = Probability.RNG.Value
    let hs = [|for i in 1..n -> hbase + rnd.NextDouble() * hrange|]
    let rs = [|for i in 1..n -> rbase + rnd.NextDouble() * rrange|]
    let ls = [| for i in 1..n -> [|for j in 1..dims -> -1.0 + 2.0 * rnd.NextDouble()|] |]
    [| for i in 0..n-1 -> {H=hs.[i]; R=rs.[i]; L=ls.[i]} |]
 
//scaling parameters from java code
let Hstepscale = 0.33
let Rstepscale = 0.33
let cstepscale = 0.2 

let lgst a x = seq{yield x; yield! x |> Seq.unfold(fun x -> let n = a*x*(1.0 - x) in Some(n,n))}
let lgstEnum a x = let s = lgst a x |> Seq.cache in s.GetEnumerator()

let flip () = Probability.RNG.Value.NextDouble() < 0.5

let changeLocation (enum:IEnumerator<float>) cone = 
    let newL = cone.L |> Array.map (fun l ->
        let dl = enum.MoveNext() |> ignore; enum.Current
        let dl = dl * cstepscale
        let dl = if flip() then dl else dl * -1.0
        let l  = l + dl
        if l > 1.0 then
            l - 2.0 * dl
        else 
            l)
    {cone with L=newL}

let changeHeight (enum:IEnumerator<float>) maxH cone =
    let dh = enum.MoveNext() |> ignore; enum.Current
    let dh = dh * Hstepscale
    let dh = if flip() then dh else dh * -1.0
    let hpct = cone.H / maxH
    let hpct = hpct + dh
    let hpct = if hpct > 1.0 then hpct - 2. * dh else hpct
    let newH = maxH * hpct
    {cone with H=newH}

let changeRadius (enum:IEnumerator<float>) maxR cone =
    let dr = enum.MoveNext() |> ignore; enum.Current
    let dr = dr * Rstepscale
    let dr = if flip() then dr else dr * -1.0
    let rpct = cone.R / maxR
    let rpct = rpct + dr
    let rpct = if rpct > 1.0 then rpct - 2. * dr else rpct
    let newR = maxR * rpct
    {cone with H=newR}

type World = 
    {
        Cones : Cone[] 
        Ar    : IEnumerator<float> option
        Ah    : IEnumerator<float> option
        Ac    : IEnumerator<float> option
        }

let createWorld n dims (hbase,hrange) (rbase,rrange) aR aH aC =
    let cones = genCones n dims (hbase,hrange) (rbase,rrange)
    {
        Cones = cones
        Ar    = aR |> Option.map (fun a -> lgstEnum a 0.45) //java code uses 0.45 as initial value
        Ah    = aH |> Option.map (fun a -> lgstEnum a 0.45)
        Ac    = aC |> Option.map (fun a -> lgstEnum a 0.45)
    }

let updateWorld world =
    let maxH  = world.Cones |> Array.map (fun c->c.H) |> Array.max
    let maxR  = world.Cones |> Array.map (fun c->c.R) |> Array.max
    let cones =  
        match world.Ar with 
        | Some enm -> world.Cones |> Array.map (changeRadius enm maxR)
        | None -> world.Cones
    let cones =
        match world.Ah with
        | Some enm -> cones |> Array.map (changeHeight enm maxH)
        | None -> cones
    let cones =
        match world.Ac with
        | Some enm -> cones |> Array.map (changeLocation enm)
        | None -> cones
    {world with Cones = cones}

let maxF ds m c = 
    let dist = (ds,c.L) ||> Array.map2(fun a b-> sqr (a-b)) |> Array.sum |> sqrt
    max m (c.H - c.R * dist)

let landscape world =
    let cones = world.Cones
    let maxCone = cones |> Array.maxBy (fun x -> x.H)
    let df ds = (System.Double.MinValue,cones) ||> Array.fold (maxF ds)
    maxCone,df

//let df1_2d (rnd:System.Random) n (hbase,hrange) (rbase,rrange) =
//    let hs = [|for i in 1..n -> hbase + rnd.NextDouble() * hrange|]
//    let rs = [|for i in 1..n -> rbase + rnd.NextDouble() * rrange|]
//    let xi = [|for i in 1..n -> -1.0 + 2.0 * rnd.NextDouble()|]
//    let yi = [|for i in 1..n -> -1.0 + 2.0 * rnd.NextDouble()|]
//    let nList = [for i in 0 .. n-1 -> i]
//    let (maxH,i,_) = ((0.,0,0),hs) ||> Array.fold (fun (mx,mxi,i) h -> if mx > h then (mx,mxi,i+1) else (h,i,i+1))
//    let cone = maxH,(xi.[i],yi.[i])
//    let f x y =
//        let maxF m i = max m (hs.[i] - rs.[i] * sqrt (sqr (x - xi.[i]) + sqr (y - yi.[i])))
//        (System.Double.MinValue,nList) ||> List.fold maxF
//    f,cone

//(*
//let testDfi = df1_2d (System.Random()) 5 (3.,3.) (5.,5.)
//let landscape = 
//    [for x in -1.0 .. 0.1 .. 1.0 do
//        for y in -1.0 .. 0.1 .. 1.0 do
//            yield (x,y,testDfi x y)]
//*)


open System.IO

let createDf1 file =
    let lines = File.ReadLines file |> Seq.skip 5
    let cones =
        lines
        |> Seq.filter (fun s -> s.Trim().Length > 10)
//        |> Seq.mapi(fun i s -> printfn "%d - %s" i s; s)
        |> Seq.map(fun s -> s.Split([|','; ' '; '\t'|],System.StringSplitOptions.RemoveEmptyEntries))
        |> Seq.map(fun xs -> xs |> Array.map float)
        |> Seq.map (fun xs -> {L=[|xs.[0]; xs.[1]|]; H=xs.[2]; R=xs.[3]})
        |> Seq.toArray
    let maxCone = cones |> Array.maxBy (fun x -> x.H)
    let df ds = (System.Double.MinValue,cones) ||> Array.fold (maxF ds)
    maxCone,df
(*
let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../Landscapes/test_cone3.5.csv")
let df1 = createDf1 @"C:\Users\cz8gb9\Documents\Visual Studio 2015\Projects\CALib\CALib\Landscapes\test_cone1.01.csv"
[for i in -1. .. 0.1 .. 1. do
    for j in -1. .. 0.1 .. 1. do
        yield df1 i j]
*)
        

