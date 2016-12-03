module CAUtils
open CA


let rnd = Probability.RNG

let gaussian mean sigma = Probability.GAUSS mean sigma

let zsample = Probability.ZSample

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

let clampP = function
    | F(v,mn,mx)    -> F(clamp mn mx v,mn,mx)
    | F32(v,mn,mx)  -> F32(clamp mn mx v,mn,mx)
    | I(v,mn,mx)    -> I(clamp mn mx v,mn,mx)
    | I64(v,mn,mx)  -> I64(clamp mn mx v,mn,mx)
    
let unifrmI (s:float) frmV toV mn mx = 
    frmV + (int ((rnd.Value.NextDouble()) * s * float (toV - frmV)))
    |> clamp mn mx

let unifrmF32 s (frmV:float32) (toV:float32) mn mx = 
    frmV + (float32 ((rnd.Value.NextDouble()) * s * float (toV - frmV)))
    |> clamp mn mx

let unifrmF s frmV toV mn mx = 
    frmV + (rnd.Value.NextDouble() * s * (toV - frmV))
    |> clamp mn mx

let unifrmI64 s frmV toV mn mx =  
    frmV + (int64 ((rnd.Value.NextDouble()) * s * float (toV - frmV)))
    |> clamp mn mx

let gaussI (s:int) sg = gaussian (float s) sg |> int

let gaussF32 (s:float32) sg = gaussian (float s) sg |> float32

let gaussF s sg = gaussian s sg

let gaussI64 (s:int64) sg = gaussian (float s) sg |> int64

let randomize = function
    | F (v,mn,mx)   -> F (unifrmF 1.0 mn mx mn mx, mn, mx)
    | F32 (v,mn,mx) -> F32 (unifrmF32 1.0 mn mx mn mx, mn, mx)
    | I (v,mn,mx)   -> I(unifrmI 1.0 mn mx mn mx, mn, mx)
    | I64 (v,mn,mx) -> I64(unifrmI64 1.0 mn mx mn mx, mn, mx)

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

let epsilonM = function
    | F(_,mn,mx)    -> F(0.001,mn,mx)
    | F32(_,mn,mx)  -> F32(0.001f,mn,mx)
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

let squareNetwork (pop:Population<'k>) id =
    let sz = pop.Length
    [|
        for i in -2 .. 1 .. 2 do
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

let Maximize a b = a > b
let Minimize a b = a < b

let vF   = function F(v,_,_) -> v   | _ -> failwith "invalid type"
let vF32 = function F32(v,_,_) -> v | _ -> failwith "invalid type"
let vI   = function I(v,_,_) -> v   | _ -> failwith "invalid type"
let vI64 = function I64(v,_,_) -> v | _ -> failwith "invalid type"

let toVF (v:float) mn mx = F(clamp mn mx v, mn, mx)
let toVI (v:float) mn mx = I(clamp mn mx (int v), mn, mx)
let toVF32 (v:float) mn mx = F32(clamp mn mx (float32 v), mn, mx)
let toVI64 (v:float) mn mx = I64(clamp mn mx (int64 v), mn, mx)
