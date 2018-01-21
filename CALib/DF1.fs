///Generate a test function of aribtrary complexity
///Paper: "A Test Problem Generator for Non-Stationary Environments" by Morrison & De Jong
module DF1
open Probability
open System.Collections.Generic
open FSharp.Collections.ParallelSeq

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

//logistic sequence generator
let lgst a x = seq{yield x; yield! x |> Seq.unfold(fun x -> let n = a*x*(1.0 - x) in Some(n,n))}  

//enumerate a logistic sequence
let lgstEnum a x = let s = lgst a x |> Seq.cache in s.GetEnumerator()

let flip () = Probability.RNG.Value.NextDouble() < 0.5

//copied from java code - still produces out of range values
let changeLocation2 (enum:IEnumerator<float>) cone = 
    let newL = cone.L |> Array.map (fun l ->
        let xpct = (l + 1.0) / 2.0                          //from java code; not sure why
        let cstep = enum.MoveNext() |> ignore; enum.Current
        let xtemp = cstep * cstepscale
        let xtemp = if flip() then xtemp else xtemp * -1.0
        let xpct  = xpct + xtemp
        let xptc = 
            if xpct > 1.0 then
                xpct - 2.0 * xtemp
            elif xpct < 0.0 then
                xpct + 2.0 * xtemp
            else 
                xpct
        let l = (xptc * 2.0) - 1.0
        if (xptc > -1.0 && xptc < 1.0) |> not then failwith "cone location not between -1 and +1"
        l
        )
        
    {cone with L=newL}


let private scaler (sMin,sMax) (vMin,vMax) (v:float) =
    if v < vMin then failwith "out of min range for scaling"
    if v > vMax then failwith "out of max range for scaling"
    (v - vMin) / (vMax - vMin) * (sMax - sMin) + sMin
    (*
    scaler (0.1, 0.9) (10., 500.) 223.
    scaler (0.1, 0.9) (10., 500.) 10.
    scaler (0.1, 0.9) (10., 500.) 500.
    scaler (0.1, 0.9) (-200., -100.) -110.
    *)


//new version changes all cones and rescales to [-1,+1] for each dimension
let changeLocations3 (enum:IEnumerator<float>) cones = 
    let locs = cones |> Array.map (fun c ->
        c.L |> Array.map (fun l -> 
            let cstep = enum.MoveNext() |> ignore; enum.Current
            let step = cstep * cstepscale
            let step = (if flip() then 1. else -1.) * step
            l + step))
    let dims = locs.[0].Length
    let mnmxs = //min max range for each dimension
        [for i in 0..dims-1 do
            let mn,mx = ((1.0,-1.0), locs) ||> Array.fold (fun (mn,mx) ds ->
                let v = ds.[i]
                (min mn v, max mx v))
            yield mn,mx
        ]
    let targetRange = (-1.0 + System.Double.Epsilon, 1.0 + System.Double.Epsilon)
    Array.zip cones locs
    |> Array.map (fun (c,ls) ->
        let newLs = ls |> Array.mapi (fun i l -> 
            let sRange = mnmxs.[i]
            scaler targetRange sRange  l)
        {c with L = newLs})

let changeLocations (enum:IEnumerator<float>) cones = 
    let locs = cones |> Array.map (fun c ->
        c.L |> Array.map (fun l -> 
            let l = (l + 1.0) / 2.0 // scale to [0,1] from [-1,1]
            let cstep = enum.MoveNext() |> ignore; enum.Current
            //let cstep = 0.5 - cstep   
            let step = cstep * cstepscale
            let step = (if flip() then 1. else -1.) * step //some randomness for a balanced landscape
            l + step))
    Array.zip cones locs
    |> Array.map (fun (c,ls) -> 
        let ls' = 
            ls 
            |> Array.map (fun l -> (l * 2.0) - 1.0) //rescale to [-1,1]
            |> Array.map (function
                | l when l < -1.0 ->  l + 1.0         //wrap around
                | l when l > 1.0 ->   l - 1.0
                | l -> l)
        {c with L = ls'})
       
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

//cones world is a set of cones and optional logistic enumerations for radius, height and location
type World = 
    {
        Cones : Cone[] 
        Ar    : IEnumerator<float> option
        Ah    : IEnumerator<float> option
        Ac    : IEnumerator<float> option
        }

//create world with given parameters 
let createWorld n dims (hbase,hrange) (rbase,rrange) aR aH aC =
    let cones = genCones n dims (hbase,hrange) (rbase,rrange)
    {
        Cones = cones
        Ar    = aR |> Option.map (fun a -> lgstEnum a 0.45) //java code uses 0.45 as initial value
        Ah    = aH |> Option.map (fun a -> lgstEnum a 0.45)
        Ac    = aC |> Option.map (fun a -> lgstEnum a 0.45)
    }

//update world cone parameters using logictic enumerations specified
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
        | Some enm -> cones |> changeLocations enm
        | None -> cones
    {world with Cones = cones}

//find the maximum height of the cones world at a given location
let maxF ds m c = 
    let dist = (ds,c.L) ||> Array.map2(fun a b-> sqr (a-b)) |> Array.sum |> sqrt
    max m (c.H - c.R * dist)

//get the fitness function and max hight cone for a given world
let landscape world =
    let cones = world.Cones
    let maxCone = cones |> Array.maxBy (fun x -> x.H)
    let fitness ds = (System.Double.MinValue,cones) ||> PSeq.fold(maxF ds)
    maxCone,fitness

(*
#load "Probability.fs"
#r @"..\packages\FSharp.Collections.ParallelSeq\lib\net40\FSharp.Collections.ParallelSeq.dll"
#load "DF1.fs"
open DF1
let w = createWorld 500 2 (5.,15.) (20., 10.) None None (Some 3.1) |> ref
for i in 1 .. 1000000 do
    w := updateWorld !w
    let valid = 
        w.Value.Cones 
        |> Seq.forall (fun c-> c.L |> Seq.forall (fun l -> 
            if l >= -1.0 && l <= 1.0 then
                true
            else
                printfn "%A" c
                false
            ))
    if not valid then printfn "not valid"

*)

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
        

