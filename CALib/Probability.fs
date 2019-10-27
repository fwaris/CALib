///Random number generation and sampling functions
module Probability
open System
open System.Threading

//xorshift128plus implementation, https://en.wikipedia.org/wiki/Xorshift
///fast PRNG (pseudo random number generator)
type XorshiftPRNG(seed) =
    let mutable s : uint64[] = Array.zeroCreate 2

    do s.[1] <- uint64 seed

    let sample() =
        let mutable x = s.[0]
        let y = s.[1]
        s.[0] <- y
        x <- x ^^^ (x <<< 23)
        s.[1] <- x ^^^ y ^^^ (x >>> 17) ^^^ (y >>> 26)
        let smpl = s.[1] + y
        if smpl = System.UInt64.MaxValue then smpl - 1UL else smpl

    member x.NextDouble() = (float (sample())) / float System.UInt64.MaxValue

    member x.Next(max) = 
        if max < 0 then failwith "max < 0"
        x.NextDouble() * (float max) |> int

    member x.Next(min:int,max:int) = 
        if min > max then failwith "min > max" 
        let r = max - min in (float r) * (x.NextDouble()) + (float min) |> int

    member x.Next(min:float,max:float) = 
        if min > max then failwith "min > max" 
        let r = max - min in (float r) * (x.NextDouble()) + (float min) 

    new()=XorshiftPRNG(System.Environment.TickCount)

///thread local storage of PRNG so that it can be used with parallel processing code
let RNG =
    // Create master seed generator and thread local value
    let seedGenerator = new Random()
    let localGenerator = new ThreadLocal<XorshiftPRNG>(fun _ -> 
        lock seedGenerator (fun _ -> 
        let seed = seedGenerator.Next()
        new XorshiftPRNG(seed)))
    localGenerator

///Marsaglia polar method for sampling from a Z distribution
let private gaussian() = 
    let mutable v1 = 0.
    let mutable v2 = 0.
    let mutable s = 2.
    while s >= 1. || s = 0. do
        v1 <- 2. * RNG.Value.NextDouble() - 1.
        v2 <- 2. * RNG.Value.NextDouble() - 1.
        s <- v1 * v1 + v2 * v2
    let polar = sqrt(-2.*log(s) / s)
    polar,v1,v2

let private spare = new ThreadLocal<_>(fun () -> ref None)

///thread-safe Z sampler
let ZSample() = 
    match spare.Value.Value with
    | None -> 
        let polar,v1,v2 = gaussian()
        spare.Value := Some (polar,v2)
        v1*polar
    | Some(polar,v2) -> 
        spare.Value := None
        v2*polar

///thread-safe gaussian sampler
let GAUSS mean sigma = 
    match spare.Value.Value with
    | None -> 
        let polar,v1,v2 = gaussian()
        spare.Value := Some (polar,v2)
        v1*polar*sigma + mean
    | Some(polar,v2) -> 
        spare.Value := None
        v2*polar*sigma + mean

///construct a sampling wheel from given key and weight combinations
let createWheel (weights:('a*float)[]) = //key * weight  key must be unique
    let s = Array.sumBy snd weights
    let weights = if s = 0. then weights |> Array.map(fun (a,_)->a,1.0) else weights

    //normalize weights
    let nrmlzdWts = 
        weights 
        |> Array.filter (fun (_,w) -> w > 0. && (Double.IsNaN w || Double.IsInfinity w) |> not) 
        |> Array.map (fun (k,w) -> k, w / s)        //total sums to 1 now
        |> Array.sortBy snd                         //arrange ascending

    //construct cumulative distribution for sampling
    let cdf = 
        if weights.Length <= 1 then
            nrmlzdWts
        else
            (nrmlzdWts.[0],nrmlzdWts.[1..])||>Array.scan (fun (_,acc) (k,w) -> k,acc + w)
    nrmlzdWts,cdf

///sample from the wheel constructed in createWheel
let spinWheel wheel = 
    let r = RNG.Value.NextDouble()
    wheel |> Array.pick(fun (k,w) -> if w > r then Some k else None)

