module KDIPDGame
open CA
open CAUtils
open KDContinousStrategyGame
open FSharp.Collections.ParallelSeq

let MAIN_KS_INFLUENCE = 1.0
let mutable KS_ATTRACTION_COFF = 4.
let mutable IMPROVE_DEFECT_COFF = -5.0

type IpdKS = Knowledge * Map<Knowledge,float> //each indv has a primary ks and partial influece KSs
type W = Set<Id> * float
type Action = W list
type Payout = Action

type IpdState = {SumDiversity:float; NormalizedFit:float array; VMin:float; VMax:float; P1Fit:float array; P2Fit:float array; KSCount:Map<Knowledge,float>}

let parmDiversity p1 p2 = 
    (p1,p2)
    ||> Array.map2 (fun a b -> parmDiff a b |> parmToFloat |> abs) 
    |> Array.sum

//est. avg diversity of pop via sampling
let sampleAvgDiversity (pop:Population<_>) =
    let sampleSize = 30
    let rec loop acc c =
        if c >= sampleSize 
            then acc / float sampleSize
        else
            let i1 = rnd.Value.Next(0,pop.Length-1)
            let i2 = rnd.Value.Next(0,pop.Length-1)
            let p1 = pop.[i1]
            let p2 = pop.[i2]
            let d = parmDiversity p1.Parms p2.Parms
            loop (acc + d) (c + 1)
    loop 0. 0

let isExplorative = function Situational | Historical | Normative -> true | _ -> false
let isExploitative = function Domain  -> true | _ -> false

let cooperation 
    st
    neighbor 
    indv =  
    let ids = set[indv.Id; neighbor.Id]
    let d = parmDiversity indv.Parms neighbor.Parms / st.SumDiversity  //normalize diversity
    let fNbr = st.NormalizedFit.[neighbor.Id]
    let fI = st.NormalizedFit.[indv.Id]
    let pf1I = st.P1Fit.[indv.Id]
    let pf2I = st.P2Fit.[indv.Id]
    let (ksI,md) = indv.KS
    let (ksN,_) = neighbor.KS
    let domainBoost = match md |> Map.tryFind Domain  with Some x when ksN = Domain -> x | _ -> 0.
    let nKSC = 1. - st.KSCount.[ksN] //level of ks
    let coop =
        let fiImprov = fI - pf1I
        let attraction = (fNbr - fI) // |> max 0.
        let sameKs = if ksI = ksN then -2.0 else 0.
        let defectCoof =  if isExploitative ksI && (fI > pf1I) then IMPROVE_DEFECT_COFF else 0.
        let ksCompatibility = if isExplorative ksI && isExploitative ksN then KS_ATTRACTION_COFF else 0.
        let fitImprvFactor = st.P1Fit.[indv.Id] - fI //reduce coop if fit improved under current regime
        let c = d + attraction + ksCompatibility + fitImprvFactor + defectCoof + (nKSC * 4.0) + sameKs
        c//System.Math.Tanh(c)
    ids,coop

let relativeCoop totalCoop (s,coop) = s, if coop < 0. then 0. else coop / totalCoop

let play state _ indv neighbors payoff : Action =
    let coops = neighbors |> Seq.map (cooperation state indv) |> Seq.toList
    let sumCoops = coops |> List.sumBy (fun (_,c) -> max c 0.)
    let ws = coops |> List.map (relativeCoop sumCoops)
//    if ws |> List.exists(fun (_,f) -> f < 0. && f > 1.) then printfn "******** %A" ws
//    let ws2 = ws |> List.toArray 
//    printfn "%A" ws2
    ws

let payoff _ _ indv indvActn (nhbrActns:Action seq) : Payout =
    let m1 = indvActn |> Map.ofList
    let payoff = 
        nhbrActns 
        |> Seq.choose (fun acts -> 
            acts 
            |> List.tryPick (fun (s,c1) ->
                m1 
                |> Map.tryFind s
                |> Option.map (fun c2 -> s, c1+c2))
            )
        |> Seq.toList
    if payoff.Length <> (Seq.length nhbrActns) then
        failwithf "neighbor action counts not matched %A %A" indv nhbrActns
    payoff

let normalizePopFitness target cmprtr (pop:Individual<_>[]) =
    let sign = if cmprtr 2. 1. then 1. else -1.
    let currentFit = pop |> Array.Parallel.map (fun p -> p.Fitness * sign) //converts minimization to maximization (higher fitness is better)
    let minFit = currentFit |> PSeq.min
    let maxFit = currentFit |> PSeq.max
    let scaler = CAUtils.scaler target (minFit,maxFit) 
    currentFit |> Array.Parallel.map scaler  //scale fitness to target range

let other i s = if Set.minElement s = i then Set.maxElement s else Set.minElement s

//let VMAX = 01.75 //payout is between 0 and 2 
//let VMIN = 0.25 


let updateKsw (pop:Population<IpdKS>) payout indv =
    let ksw = 
        payout 
        |> List.map (fun (idSet,f) ->  //set<id>, float [0-2]
            let nhbrId = other indv.Id idSet
            let (ks,_) = pop.[nhbrId].KS
            ks,f / 2.)    //payout is in range [0., 2.] so normalize to max 1.0
        |> List.groupBy fst
        |> List.map (fun (ks,fs)->
            ks,
            fs |> List.sumBy snd |> min 1.0    //cap alt KS influence to 1.0
            )
        |> Map.ofList
    ksw

let removePrimaryKS ks m = Map.remove ks m
let removeSituationalKS m = Map.remove Situational m //Situational cannot be secondary KS as it changes the indv completely
let promoteDomainKS = function (Domain,_) as k -> k |(_,m) when m |> Map.containsKey Domain -> Domain,Map.empty | k -> k

let createKs primary others = (primary, removePrimaryKS primary others |> removeSituationalKS) //|> promoteDomainKS

let updateIndv (vmin,vmax) cmprtr (pop:Population<IpdKS>) indv payout =
    let payout = payout |> List.filter (fun (_,f) -> f > vmin)
    let vmx = payout |> List.filter (fun (_,f) -> f >= vmax)
    let indv = 
        match vmx.Length,payout.Length with
        | 0,0 -> {indv with KS=fst indv.KS,Map.empty}
        | 0,_ -> let ks = createKs (fst indv.KS) (updateKsw pop payout indv)
                 {indv with KS=ks}
        | 1,_ ->
            let nhbr = pop.[other indv.Id (fst vmx.[0])]
            let (ks,_):IpdKS = nhbr.KS
//            printfn "%A->%A" (fst indv.KS) (ks)
            //let ksw = updateKsw pop payout indv 
            {indv with KS = createKs ks Map.empty}
        | _,_ ->  
            let i = CAUtils.rnd.Value.Next(0,vmx.Length - 1)
            let nhbr = pop.[other indv.Id (fst vmx.[i])]
            let (ks,_):IpdKS = nhbr.KS
//            printfn "*%A->%A" (fst indv.KS) (ks)
            {indv with KS = createKs ks Map.empty}  
    let (p,_) = indv.KS
    //if p=Domain then printfn "%d %A" indv.Id indv.KS
    indv

let updatePop vmx cmprtr pop (payouts:Payout array) = 
    let pop = pop |> Array.Parallel.map(fun indv ->
        updateIndv vmx cmprtr pop indv payouts.[indv.Id]
    )
    pop

let createState prevFitOpt (vmin,vmax) cmprtr pop =
    let nrmlzdFit = normalizePopFitness (0., 1.0) cmprtr pop
    let ksc = pop |> Seq.collect(fun i-> let (k,m) = i.KS in (k,1.0)::(Map.toList m)) 
    let totalKsc = ksc |> Seq.sumBy snd
    let ksc = ksc |> Seq.map (fun (k,v) -> k, v / totalKsc) |> Map.ofSeq
    {
        SumDiversity = sampleAvgDiversity pop * float pop.Length
        NormalizedFit = nrmlzdFit
        VMin = vmin
        VMax = vmax
        P1Fit = match prevFitOpt with Some (f,_) -> f | None -> nrmlzdFit
        P2Fit = match prevFitOpt with Some (_,p) -> p | None -> nrmlzdFit
        KSCount = ksc
    }

let rec outcome state cmprtr (pop,beliefSpace) (payouts:Payout array) =
    let vmx = (state.VMin, state.VMax)
    let pop = updatePop vmx cmprtr pop payouts
    let state = createState (Some (state.NormalizedFit,state.P1Fit)) vmx cmprtr pop
    pop,
    beliefSpace,
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state
    }

let initKS (pop:Population<Knowledge>) = 
    pop 
    |> Array.Parallel.map (fun indv -> 
        {
            Id = indv.Id
            Fitness = indv.Fitness
            Parms = indv.Parms
            KS=indv.KS,Map.empty
        })

let game (vmin,vmax) cmprtr pop =
    let state = createState None (vmin,vmax) cmprtr pop
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state
    }

let ipdInfluence beliefSpace pop :Population<IpdKS> =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> Map.ofList
    let pop =
        pop
        |> Array.Parallel.map (fun p -> 
            let mainKs,otherKs = p.KS
            let p = ksMap.[mainKs].Influence MAIN_KS_INFLUENCE p
            (p,otherKs) ||> Map.fold (fun p k w -> ksMap.[k].Influence w p))
    pop 

let knowledgeDist (vmin,vmax) comparator pop =
    let g = game (vmin,vmax)  comparator pop
    KDContinousStrategyGame.knowledgeDist comparator g

