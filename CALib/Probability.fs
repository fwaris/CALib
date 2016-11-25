module Probability
open System
open System.Threading
//thread-safe random number generator
let RNG =
    // Create master seed generator and thread local value
    let seedGenerator = new Random();
    let localGenerator = new ThreadLocal<Random>(fun _ -> 
        lock seedGenerator (fun _ -> 
        let seed = seedGenerator.Next()
        new Random(seed)))
    localGenerator

//Marsaglia polar method
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

//thread-safe gaussian sampler
let GAUSS mean sigma = 
    match spare.Value.Value with
    | None -> 
        let polar,v1,v2 = gaussian()
        spare.Value := Some (polar,v2)
        v1*polar*sigma + mean
    | Some(polar,v2) -> 
        spare.Value := None
        v2*polar*sigma + mean

(*
#load "Probability.fs"
open Probability
let reqs =  [for i in 1 .. 1000 -> async{return (GAUSS 10. 3.)}]
let rs = Async.Parallel reqs |> Async.RunSynchronously
*)
