module CAUtils
open CA

module Probability =
    open System
    open System.Threading
    let RNG =
        // Create master seed generator and thread local value
        let seedGenerator = new Random()
        let localGenerator = new ThreadLocal<Random>(fun _ -> 
            lock seedGenerator (fun _ -> 
            let seed = seedGenerator.Next()
            new Random(seed)))
        localGenerator
     
let rnd = Probability.RNG

//Marsaglia polar method
let gaussian mean sigma = 
    let mutable v1 = 0.
    let mutable v2 = 0.
    let mutable s = 2.
    while s >= 1. || s = 0. do
        v1 <- 2. * rnd.Value.NextDouble() - 1.
        v2 <- 2. * rnd.Value.NextDouble() - 1.
        s <- v1 * v1 + v2 * v2
    let polar = sqrt(-2.*log(s) / s)
    v1*polar*sigma + mean
(*
let rnd = System.Random()
[for i in 0..100 -> gaussian (float 50.) 1.]
*)

let inline yourself x = x

let scaler (sMin,sMax) (vMin,vMax) (v:float) =
    if v < vMin then failwith "out of min range for scaling"
    if v > vMax then failwith "out of max range for scaling"
    (v - vMin) / (vMax - vMin) * (sMax - sMin) + sMin
    (*
    scaler (0.1, 0.9) (10., 500.) 223.
    scaler (0.1, 0.9) (10., 500.) 10.
    scaler (0.1, 0.9) (10., 500.) 500.
    scaler (0.1, 0.9) (-200., -100.) -110.
    *)

//function to turn minimzation problems into maximization for solving
let maximization compartor =  if compartor 2. 1. then fun x -> x * -1. else fun x -> x

let flatten tree =
    let rec loop acc = function
        | Roots roots       -> List.fold loop acc roots
        | Node(ks,children) -> List.fold loop (ks::acc) children
        | Leaf(leaf)        -> leaf::acc
    loop [] tree
        
let clamp mn mx x = max (min x mx) mn

let randI (s:float) frmV toV mn mx = 
    frmV + (int ((rnd.Value.NextDouble()) * s * float (toV - frmV)))
    |> clamp mn mx

let randF32 s (frmV:float32) (toV:float32) mn mx = 
    frmV + (float32 ((rnd.Value.NextDouble()) * s * float (toV - frmV)))
    |> clamp mn mx

let randF s frmV toV mn mx = 
    frmV + (rnd.Value.NextDouble() * s * (toV - frmV))
    |> clamp mn mx

let randI64 s frmV toV mn mx =  
    frmV + (int64 ((rnd.Value.NextDouble()) * s * float (toV - frmV)))
    |> clamp mn mx

let randomize = function
    | F (v,mn,mx)   -> F (randF 1.0 mn mx mn mx, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 1.0 mn mx mn mx, mn, mx)
    | I (v,mn,mx)   -> I(randI 1.0 mn mx mn mx, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 1.0 mn mx mn mx, mn, mx)

let baseKsInit beliefSpace = 
    let kss = flatten beliefSpace |> List.toArray
    fun i -> kss.[i % kss.Length].Type

let ksSetInit beliefSpace = 
    let kss = flatten beliefSpace |> List.toArray
    fun i -> set[kss.[i % kss.Length].Type]

let createPop ksInitializer parms size randomizeAll =
    let rnd = System.Random()
    [|
        for i in 1..size do
            let parms = 
                //if specified, use the inital parm values for some inds (i.e. don't randomize)
                if i < 5 && not randomizeAll then 
                    parms 
                else
                    parms |> Array.map randomize
            yield
                {
                    Id      = i-1
                    Parms   = parms
                    Fitness = System.Double.MinValue
                    KS      = ksInitializer i
                }
    |]

type Dir = Up | Down | Flat

//add two parms
let parmAdd p1 p2 = 
    match p1, p2 with
    | F(prevV,_,_),F(newV,mn,mx)        -> F(prevV + newV   ,mn,mx)
    | F32(prevV,_,_),F32(newV,mn,mx)    -> F32(prevV + newV ,mn,mx)
    | I(prevV,_,_),I(newV,mn,mx)        -> I(prevV + newV   ,mn,mx)
    | I64(prevV,_,_),I64(newV,mn,mx)    -> I64(prevV + newV ,mn,mx)
    | a,b -> failwithf "CAUtils: invalid combination of types for parmSum %A,%A" a b

//Effectively newParm - oldParm
let parmDiff newParm oldParm  = 
    match oldParm, newParm with
    | F(prevV,_,_),F(newV,mn,mx)        -> F(newV - prevV   ,mn,mx)
    | F32(prevV,_,_),F32(newV,mn,mx)    -> F32(newV - prevV ,mn,mx)
    | I(prevV,_,_),I(newV,mn,mx)        -> I(newV - prevV   ,mn,mx)
    | I64(prevV,_,_),I64(newV,mn,mx)    -> I64(newV - prevV ,mn,mx)
    | a,b -> failwithf "CAUtils: invalid combination of types for parmDiff %A,%A" a b

//Effectively numerator / denominator
let parmToFloat = function
    | F(v,_,_)   -> v
    | F32(v,_,_) -> float v
    | I(v,_,_)   -> float v
    | I64(v,_,_) -> float v

let epsilon = function
    | F(_,mn,mx)    -> F(System.Double.Epsilon,mn,mx)
    | F32(_,mn,mx)  -> F32(System.Single.Epsilon,mn,mx)
    | I(_,mn,mx)    -> I(1,mn,mx)
    | I64(_,mn,mx)  -> I64(1L,mn,mx)

let lBestNetwork (pop:Population<'k>) id = //return 2 'friends' from the ring
    let m1 = id - 1
    let m1 = if m1 < 0 then pop.Length + m1 else m1
    let p1 = id + 1 
    let p1 = if p1 >= pop.Length then p1 - pop.Length else p1
//    printfn "id=%d, m1=%d, m2=%d, p1=%d, p2=%d" id m1 m2 p1 p2
    [|pop.[m1]; pop.[p1]; |]
(*
#load "CA.fs"
open CA
let parms = [|F(1.,1.,10.)|]
let pop= [|for i in 1..100 -> {Id=i;Parms=parms;Fitness=0.;KS=Normative}|]
let net = pop |> Array.mapi (fun i _ -> lBestNetwork pop i)       
*)
let l4BestNetwork (pop:Population<'k>) id = //return 4 'friends' from the ring
    let m2 = id - 2
    let m2 = if m2 < 0 then pop.Length + m2 else m2
    let m1 = id - 1
    let m1 = if m1 < 0 then pop.Length + m1 else m1
    let p2 = id + 2
    let p2 = if p2 >= pop.Length then p2 - pop.Length else p2
    let p1 = id + 1 
    let p1 = if p1 >= pop.Length then p1 - pop.Length else p1
//    printfn "id=%d, m1=%d, m2=%d, p1=%d, p2=%d" id m1 m2 p1 p2
    [|pop.[m2]; pop.[m1]; pop.[p1]; pop.[p2] |]

let Maximize a b = a > b
let Minimize a b = a < b

let vF   = function F(v,_,_) -> v   | _ -> failwith "invalid type"
let vF32 = function F32(v,_,_) -> v | _ -> failwith "invalid type"
let vI   = function I(v,_,_) -> v   | _ -> failwith "invalid type"
let vI64 = function I64(v,_,_) -> v | _ -> failwith "invalid type"


