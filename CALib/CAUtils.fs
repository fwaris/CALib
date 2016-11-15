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
        
let randI min max = rnd.Value.Next(min,max)
let randF32 (min:float32) (max:float32) =  min + (float32 ((rnd.Value.NextDouble()) * float (max - min)))
let randF  min max = min + (rnd.Value.NextDouble() * (max - min))
let randI64 min max =  min + (int64 ((rnd.Value.NextDouble()) * float (max - min)))
(*
let rnd = System.Random()
[for i in 1..100 -> randI 1 1000]
[for i in 1..100 -> randF32 1.f 1000.f]
[for i in 1..100 -> randF 1000. 1000000.]
[for i in 1..100 -> randI64 1000L 1000000L]
*)   

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

let randomize = function
    | F (v,mn,mx)   -> F (randF mn mx, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 mn mx, mn, mx)
    | I (v,mn,mx)   -> I(randI mn mx, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 mn mx, mn, mx)

let slideUp = function
    | F (v,mn,mx)   -> F (randF v mx, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 v mx, mn, mx)
    | I (v,mn,mx)   -> I(randI v mx, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 v mx, mn, mx)

let slideDown = function
    | F (v,mn,mx)   -> F (randF mn v, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 mn v, mn, mx)
    | I (v,mn,mx)   -> I(randI mn v, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 mn v, mn, mx)

let clamp mn mx x = max (min x mx) mn

let evolveInt iV =
    let v = float iV
    let v' = gaussian v 3.
    if abs (v' - v) > 1. then 
        int v' 
    elif v'<v then 
        iV - 1 
    else 
        iV + 1 

let evolveInt64 i64V =
    let v  = float i64V
    let v' = gaussian v 3.
    if abs (v' - v) > 1. then 
        int64 v' 
    elif v' < v then 
        i64V - 1L
    else 
        i64V + 1L 

let evolveS = function
    | F (v,mn,mx)    -> F   (gaussian v 1.                      |> clamp mn mx, mn, mx)
    | F32 (v,mn,mx)  -> F32 (gaussian (float v) 1. |> float32   |> clamp mn mx, mn, mx)
    | I (v,mn,mx)    -> I   (evolveInt v                        |> clamp mn mx, mn, mx)
    | I64 (v,mn,mx)  -> I64 (evolveInt64 v                      |> clamp mn mx, mn, mx)

///Use values from the 2nd parm to influence 1st parm
///(randomly move towards 2nd parm value)
let influenceParm influenced influencer =
    match influencer,influenced with
    | F(pV,mn,mx),F(iV,_,_) when pV > iV     -> F(randF iV pV,mn,mx)
    | F(pV,mn,mx),F(iV,_,_) when pV < iV     -> F(randF pV iV,mn,mx)
    | F(_),fInd                              -> evolveS fInd

    | F32(pV,mn,mx),F32(iV,_,_) when pV > iV -> F32(randF32 iV pV,mn,mx)
    | F32(pV,mn,mx),F32(iV,_,_) when pV < iV -> F32(randF32 pV iV,mn,mx)
    | F32(_),fInd                            -> evolveS fInd

    | I(pV,mn,mx),I(iV,_,_) when pV > iV     -> I(randI iV pV,mn,mx)
    | I(pV,mn,mx),I(iV,_,_) when pV < iV     -> I(randI pV iV,mn,mx)
    | I(_),fInd                              -> evolveS fInd

    | I64(pV,mn,mx),I64(iV,_,_) when pV > iV -> I64(randI64 iV pV,mn,mx)
    | I64(pV,mn,mx),I64(iV,_,_) when pV < iV -> I64(randI64 pV iV,mn,mx)
    | I64(_),fInd                            -> evolveS fInd

    | a,b -> failwithf "two pop individual parameters not matched %A %A" a b

///influenced indivual's parameters are modified 
///to move them towards the influencer's parameters
let influenceInd influenced influencer =
    {influenced with
        Parms = (influenced.Parms,influencer.Parms) ||> Array.map2 influenceParm
    }

///influenced indivual's parameters are modified 
///to move them towards the influencer's parameters
let evolveInd individual =
    {individual with
        Parms = individual.Parms |> Array.map evolveS
    }

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
let parmDiv numerator denominator  = 
    try
        let r = 
            match numerator, denominator with
            | F(n,_,_),F(d,_,_)        -> n / d
            | F32(n,_,_),F32(d,_,_)    -> float n / float d
            | I(n,_,_),I(d,_,_)        -> float n / float d
            | I64(n,_,_),I64(d,_,_)    -> float n / float d
            | a,b -> failwithf "CAUtils: invalid combination of types for parmDiv %A,%A" a b
        Some r
    with :? System.DivideByZeroException ->
        None

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


