﻿module SituationalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 0.05

type State = 
    {
        Exemplars    : Marker array
        SpinWheel    : (int*float)array
        ParmDefs     : Parm[]
        IsBetter     : Comparator
        MaxExemplars : int
    }

let fitweights isBetter (exemplars:Marker array) =
    let tx m = if isBetter 2. 1. then m else if m = 0.0 then 1.0 else  1.0 / m
    let wghts = exemplars |> Array.map (fun i-> tx i.MFitness)
    wghts 

let parmDiversity p1 p2 = 
    (p1,p2)
    ||> Array.map2 (fun a b -> a - b |> abs)
    |> Array.sum

let makeBins (mn:float) mx bins = 
    if mn > mx then failwith "mn > mx"
    let r = mx - mn
    let bw = r / bins
    (mn,mn+bw,1) |> Seq.unfold (fun (mn,mx,c) -> 
        if c > int bins then
            None
        else
            Some ((mn,mx), (mx,mx+bw,c+1)))
    |> Seq.toList

// makeBins 1. 17. 5.
// makeBins 1. 1.03 5.
// makeBins -10. -2.5 5.

//round robbin collect elements in the bins
//collect 1st element from 1st bin, 1st element from 2nd bin until no more bins
//then repeat starting from the 1st bin and 2nd element...
let rec clct acc l1 l2 =
    match l1, l2 with 
    | [],[]             -> acc |> List.rev
    | [],_              -> clct acc (List.rev l2) []
    | (_,[])::rest,_    -> clct acc rest l2
    | (b,x::r1)::rest,_ -> clct (x::acc) rest ((b,r1)::l2)

// clct [] ['a',[1;2]; 'b',[3;4]; 'c',[5;6]] []

let log exmplrs = exmplrs |> Seq.map (fun e -> e.MParms) |> Seq.toList |> Metrics.MetricMsg.SitState |> Metrics.postAll

let pickExamplars isBetter (prevE:Marker[]) (voters:Individual<_> seq) =
    let tx = if isBetter 2. 1. then -1. else 1.
    let voters = voters |> Seq.map (fun indv -> toMarker indv)
    let ex = Seq.append prevE  voters |> Seq.toArray
    let ex = Array.sortBy (fun x -> tx * x.MFitness) ex
    let best = ex.[0]
    let within10Pct = best.MFitness * 0.15
//    let ex = ex.[1..] |> Array.filter(fun i -> i.Fitness - best.Fitness |> abs <= within10Pct) //all others, i.e. not best
    let ex = ex.[1..] 
    let divsM = ex |> Array.mapi (fun i indv -> i, parmDiversity best.MParms indv.MParms) |> Map.ofArray
    let (_,mxD) = divsM |> Map.toArray |> Seq.maxBy snd
    let (_,mnD) = divsM |> Map.toArray |> Seq.minBy snd
    let bins = makeBins mnD (mxD + 0.0001) 5.
    let binned = (Map.empty,ex |> Array.mapi (fun i exm ->i,exm)) ||> Array.fold (fun acc (i,exm) -> 
        let d = divsM.[i]
        let b = bins |> List.find (fun (mn,mx) -> mn <= d && d < mx)
        match acc |> Map.tryFind b with
        | Some ls -> exm::ls
        | None    -> [exm]
        |> fun x -> acc |> Map.add b x
        )
    let binned = //sort each bin by decreasing order of fitness
        binned 
        |> Map.map (fun k v -> v |> List.sortBy (fun exm -> tx * exm.MFitness))
        |> Map.toSeq
        |> Seq.sortBy (fun ((mn,mx),_) -> mx) //sort all bins by decreasing diversity
        |> Seq.toList
    best::(clct [] binned [])    

let construct state fAccept fInfluence : KnowledgeSource<_> =
    {
        Type        = Situational
        Accept      = fAccept fInfluence state
        Influence   = fInfluence state
    }

let initialState parmDefs isBetter maxExemplars = 
    {
        Exemplars   = [||]
        SpinWheel   = [||]
        ParmDefs    = parmDefs
        IsBetter    = isBetter
        MaxExemplars = maxExemplars
    }

let defaultInfluence state influenceLevel (ind:Individual<_>) =
    match state.Exemplars with
    | [||] -> ind
    | x -> 
        let i = Probability.spinWheel state.SpinWheel
        let choosen = x.[i]
//            printfn "sit i = %d %A" i choosen
        ind.Parms |> Array.iteri (fun i p -> evolveP CAEvolve.RANGE_SCALER influenceLevel eSigma ind.Parms i state.ParmDefs.[i] p)
        ind

let rec defaultAcceptance 
    fInfluence 
    state
    envChanged
    (voters : Individual<_> array) =

    let state =
        match Settings.TrackEnv, envChanged with
        | true,true -> {state with Exemplars=[||]; SpinWheel=[||]} 
        | _         -> state

    let explrs = pickExamplars state.IsBetter state.Exemplars voters |> List.truncate state.MaxExemplars |> List.toArray
//        for e in explrs do printf "%0.2f [%A]" e.Fitness (parmToFloat e.Parms.[0] ,parmToFloat e.Parms.[1] )
//        printfn ""
    let weights = fitweights state.IsBetter explrs |> Array.mapi (fun i x -> i,x)
    let _,wheel = Probability.createWheel weights
    let state = {state with Exemplars=explrs; SpinWheel=wheel}

    #if _LOG_
    log explrs
    #endif

    voters, construct state defaultAcceptance fInfluence

let create parmDefs isBetter maxExemplars =
    let state = initialState parmDefs isBetter maxExemplars
    construct state defaultAcceptance defaultInfluence
