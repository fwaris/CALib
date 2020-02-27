/// Utility functions to support CA operation
module CAUtils
open CA
open FSharp.Collections.ParallelSeq

let rnd = Probability.RNG

let gaussian mean sigma = Probability.GAUSS mean sigma

let zsample = Probability.ZSample

let inline yourself x = x

let isValidNum n = (System.Double.IsInfinity n || System.Double.IsNaN n) |> not

///Fitness value multiplier depending on whether its a minimzation or maximization problem.
///Scales fitness so that higher is better
let mult = function Minimize -> -1.0 | Maximize -> 1.0

///older method of comparing fitness (still used but should move to mult)
let comparator = function Maximize -> (fun a b -> a > b) | Minimize  -> (fun a b -> a < b)
type Comparator = float -> float -> bool

///Linearly scale a value (which falls within vMin and vMax) to be within sMin and sMax
let scaler (sMin,sMax) (vMin,vMax) (v:float) =
    let v = max v vMin
    let v = min v vMax
    (v - vMin) / (vMax - vMin) * (sMax - sMin) + sMin
    (* examples
    scaler (0.1, 0.9) (10., 500.) 223.
    scaler (0.1, 0.9) (10., 500.) 10.
    scaler (0.1, 0.9) (10., 500.) 500.
    scaler (0.1, 0.9) (-200., -100.) -110.
    *)

///flatten a tree into a list
let flatten tree =
    let rec loop acc = function
        | Roots roots       -> List.fold loop acc roots
        | Node(ks,children) -> List.fold loop (ks::acc) children
        | Leaf(leaf)        -> leaf::acc
    loop [] tree
        
///clamp float value to be within min and max range supplied
//let clamp mn mx x = max (min x mx) mn                   
let clamp mn mx x = x |> min mx |> max mn

///clamp int value to be within min and max range supplied
//let clampI mn mx x = max (min (int x) mx) mn |> float  
let clampI mn mx x = (int x) |> min mx |> max mn |> float

///clamp value to be within min and max range as specified by 
///the corresponding Parm
let clampP v = function
    | F(_,mn,mx) -> clamp mn mx v 
    | I(_,mn,mx) -> clampI mn mx v

let fixParam = function
    | F(v,mn,mx) -> F(clamp mn mx v,mn,mx)
    | I(v,mn,mx) -> I(clampI mn mx (float v) |> int, mn,mx)

///Sample from uniform distribution (float)    
let unifrmF scale frmV toV = 
    frmV + (rnd.Value.NextDouble() * scale * (toV - frmV))

///Sample from uniform distribution (int)
let unifrmI (scale:float) frmV toV = 
    let v = unifrmF scale (float frmV) (float toV)
    int v 

///Select a random value using uniform distribution corresponding to the given parameter
let randomize = function
    | F (v,mn,mx)   -> F (unifrmF 1.0 mn mx, mn, mx)
    | I (v,mn,mx)   -> I (unifrmI 1.0 mn mx, mn, mx)

///Direction of change
type Dir = Up | Down | Flat

///Convert to float
let parmToFloat = function
    | F(v,_,_)   -> v
    | I(v,_,_)   -> float v

///Denominator used to find the rate of change in a parameter given its type
let denominatorM = function
    | F(_,mn,mx)    -> 0.0001
    | I(_,mn,mx)    -> 1.0


///Round robbin initializer for individual Knowledge type 
///given the Knowledge types used in the given BeliefSpace
let baseKsInit beliefSpace = 
    let kss = flatten beliefSpace |> List.toArray
    fun i -> kss.[i % kss.Length].Type

///create population
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
                    Parms   = parms |> Array.map parmToFloat
                    Fitness = System.Double.MinValue
                    KS      = ksInitializer i
                }
    |]

///Convert an Individual to a Marker
let toMarker indv = {MParms=indv.Parms |> Array.copy; MFitness=indv.Fitness}

///Network: return 2 'friends' from the ring
let lBestNetwork (pop:Population<'k>) id = //return 2 'friends' from the ring
    let m1 = id - 1
    let m1 = if m1 < 0 then pop.Length + m1 else m1
    let p1 = id + 1 
    let p1 = if p1 >= pop.Length then p1 - pop.Length else p1
    [|pop.[m1]; pop.[p1]; |]

///Network: return 4 'friends' from the ring
let l4BestNetwork (pop:Population<'k>) id = //return 4 'friends' from the ring
    let m2 = id - 2
    let m2 = if m2 < 0 then pop.Length + m2 else m2
    let m1 = id - 1
    let m1 = if m1 < 0 then pop.Length + m1 else m1
    let p2 = id + 2
    let p2 = if p2 >= pop.Length then p2 - pop.Length else p2
    let p1 = id + 1 
    let p1 = if p1 >= pop.Length then p1 - pop.Length else p1
    [|pop.[m2]; pop.[m1]; pop.[p1]; pop.[p2] |]

let hexagonNetwork (pop:Population<'k>) id =
    let sz = pop.Length
    [|
        for i in -3 .. 1 .. 3 do
            if i <> 0 then
                let idx = (id + i) % sz
                let idx = if idx < 0 then idx + sz else idx
                yield pop.[idx]
    |]

///Hexagon network where immediate neighbors 
///are the six surrounding individuals
///when the population is laid out 
///on a (visual) hexgonal grid
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

///Network: square
let squareNetwork (pop:Population<'k>) id =
    let sz = pop.Length
    [|
        for i in -2 .. 1 .. 2 do
            if i <> 0 then
                let idx = (id + i) % sz
                let idx = if idx < 0 then idx + sz else idx
                yield pop.[idx]
    |]

///Network: Octogonal
let octagonNetwork (pop:Population<'k>) id =
    let sz = pop.Length
    [|
        for i in -4 .. 1 .. 4 do
            if i <> 0 then
                let idx = (id + i) % sz
                let idx = if idx < 0 then idx + sz else idx
                yield pop.[idx]
    |]

///Return normalized fitness values from a population 
///(usually fitness is scaled to be from 0.0 to 1.0)
let normalizePopFitness target mult (pop:Individual<_>[]) =
    let currentFit = pop |> Array.Parallel.map (fun p -> p.Fitness * mult) //converts minimization to maximization (higher fitness is better)
    let minFit = currentFit |> PSeq.min
    let maxFit = currentFit |> PSeq.max
    let scaler = scaler target (minFit,maxFit) 
    currentFit |> Array.Parallel.map scaler  //scale fitness to target range

///Default CA influence function
let defaultInfluence beliefSpace pop ib =
    let ksMap = flatten beliefSpace |> List.map (fun k -> k.Type, k) |> Map.ofList
    let pop =
        pop
        |> Array.Parallel.map (fun p -> ksMap.[p.KS].Influence ib pop 1.0 p)
    pop


let initStep ca = {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0; IBest= ref None}


// track incidentally found best
module Incidental =
    let createMarker parms fit = {MParms=Array.copy parms; MFitness=fit}

    let update incidentalBest isBetter fit parms =
        match !incidentalBest with
        | Some l -> if isBetter fit l.MFitness then incidentalBest := Some (createMarker parms fit)
        | None   -> ()

    let compare incidentalBest isBetter marker =
        let betterIncidental =
            !incidentalBest
            |> Option.bind(fun l -> if isBetter l.MFitness marker.MFitness then Some l else None)

        match betterIncidental with 
        | None -> incidentalBest := Some marker
        | Some _ -> ()

        betterIncidental

    



