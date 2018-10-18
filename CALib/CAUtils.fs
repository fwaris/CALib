module CAUtils
open CA
open FSharp.Collections.ParallelSeq

let rnd = Probability.RNG

let gaussian mean sigma = Probability.GAUSS mean sigma

let zsample = Probability.ZSample

let inline yourself x = x

let isValidNum n = (System.Double.IsInfinity n || System.Double.IsNaN n) |> not


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
        
let clamp mn mx x = max (min x mx) mn                   //floats
let clampI mn mx x = max (min (int x) mx) mn |> float   //ints

let clampP = function
    | F(v,mn,mx)    -> F(clamp mn mx v,mn,mx)
    | I(v,mn,mx)    -> I(clamp mn mx v,mn,mx)

let clampP' v = function
    | F(_,mn,mx) -> clamp mn mx v 
    | I(_,mn,mx) -> clampI mn mx v

let dz f = if f > 0. then max 1.0 f else min -1.0 f
    
let unifrmF scale frmV toV = 
    frmV + (rnd.Value.NextDouble() * scale * (toV - frmV))

let unifrmI (scale:float) frmV toV = 
    let v = unifrmF scale (float frmV) (float toV)
    int v 

let gaussI (s:int) sg = gaussian (float s) sg |> int

let gaussF s sg = gaussian s sg

let randomize = function
    | F (v,mn,mx)   -> F (unifrmF 1.0 mn mx, mn, mx)
    | I (v,mn,mx)   -> I (unifrmI 1.0 mn mx, mn, mx)

type Dir = Up | Down | Flat

//add two parms
let parmAdd p1 p2 = 
    match p1, p2 with
    | F(prevV,_,_),F(newV,mn,mx)        -> F(prevV + newV   ,mn,mx)
    | I(prevV,_,_),I(newV,mn,mx)        -> I(prevV + newV   ,mn,mx)
    | a,b -> failwithf "CAUtils: invalid combination of types for parmSum %A,%A" a b

//Effectively newParm - oldParm
let parmDiff newParm oldParm  = 
    match oldParm, newParm with
    | F(prevV,_,_),F(newV,mn,mx)        -> F(newV - prevV   ,mn,mx)
    | I(prevV,_,_),I(newV,mn,mx)        -> I(newV - prevV   ,mn,mx)
    | a,b -> failwithf "CAUtils: invalid combination of types for parmDiff %A,%A" a b

//Effectively numerator / denominator
let parmToFloat = function
    | F(v,_,_)   -> v
    | I(v,_,_)   -> float v

let epsilon = function
    | F(_,mn,mx)    -> F(System.Double.Epsilon,mn,mx)
    | I(_,mn,mx)    -> I(1,mn,mx)

let epsilonM = function
    | F(_,mn,mx)    -> 0.000001
    | I(_,mn,mx)    -> 1.0

let denominatorM = function
    | F(_,mn,mx)    -> 0.0001
    | I(_,mn,mx)    -> 1.0

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
                    Parms   = parms |> Array.map randomize |> Array.map parmToFloat
                    Fitness = System.Double.MinValue
                    KS      = ksInitializer i
                }
    |]

let toMarker indv = {MParms=indv.Parms |> Array.copy; MFitness=indv.Fitness}

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

//let squareNetwork (pop:Population<'k>) id = 
//    //assume pop is in a square grid with wrap around for indv at the edges or outside the perfect square
//    let h = float pop.Length |> sqrt |> int
//    let w = h
//    let (r,c) =
//        let row = id / w
//        let col = id % h
//        row,col
//    let idx (r,c) =
//        let r = if r < 0 then (h - 1) elif r > h-1 then 0 else r
//        let c = if c < 0 then (w - 1) elif c > w-1 then 0 else c 
//        r * h + c
//    [|
//        pop.[idx (r,c-1)] //left
//        pop.[idx (r-1,c)] //top
//        pop.[idx (r,c+1)] //right
//        pop.[idx (r+1,c)] //bottom        
//    |]
//
let hexagonNetwork (pop:Population<'k>) id =
    let sz = pop.Length
    [|
        for i in -3 .. 1 .. 3 do
            if i <> 0 then
                let idx = (id + i) % sz
                let idx = if idx < 0 then idx + sz else idx
                yield pop.[idx]
    |]

let hexagonNetworkViz (pop:Population<'k>) id =
    let rowCount = sqrt (float pop.Length)
    let rowLen = int rowCount
    let r = id / rowLen
    let c = id % rowLen
    let evnCol = if c % 2 = 0 then 1 else -1
    let idxs = 
        [|
            r * rowLen + c-1
            r * rowLen + c+1
            (r-1) * rowLen + c
            (r+1) * rowLen + c
            (r+evnCol) * rowLen + (c-1)
            (r+evnCol) * rowLen + (c+1)
        |]
    idxs |> Array.map (fun i-> pop.[if i < 0 then pop.Length+i else i % pop.Length])

let squareNetwork (pop:Population<'k>) id =
    let sz = pop.Length
    [|
        for i in -2 .. 1 .. 2 do
            if i <> 0 then
                let idx = (id + i) % sz
                let idx = if idx < 0 then idx + sz else idx
                yield pop.[idx]
    |]

let octagonNetwork (pop:Population<'k>) id =
    let sz = pop.Length
    [|
        for i in -4 .. 1 .. 4 do
            if i <> 0 then
                let idx = (id + i) % sz
                let idx = if idx < 0 then idx + sz else idx
                yield pop.[idx]
    |]

(*
#load "CA.fs"
#load "Probability.fs"
#load "CAUtils.fs"
open CA
open CAUtils
let pop = [|for i in 0 .. 100 -> {Individual.Id=i; Parms=[||]; KS=Domain; Fitness=1.; } |]
squareNetwork pop 25
*)

let Maximize a b = a >= b
let Minimize a b = a <= b

let vF   = function F(v,_,_) -> v   | _ -> failwith "invalid type"
let vI   = function I(v,_,_) -> v   | _ -> failwith "invalid type"

let toVF (v:float) mn mx = F(clamp mn mx v, mn, mx)
let toVI (v:float) mn mx = I(clamp mn mx (int v), mn, mx)

let normalizePopFitness target cmprtr (pop:Individual<_>[]) =
    let sign = if cmprtr 2. 1. then 1. else -1.
    let currentFit = pop |> Array.Parallel.map (fun p -> p.Fitness * sign) //converts minimization to maximization (higher fitness is better)
    let minFit = currentFit |> PSeq.min
    let maxFit = currentFit |> PSeq.max
    let scaler = scaler target (minFit,maxFit) 
    currentFit |> Array.Parallel.map scaler  //scale fitness to target range
