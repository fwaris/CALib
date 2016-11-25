module KDIPDGame
open CA
open CAUtils
open KDContinousStrategyGame
open FSharp.Collections.ParallelSeq

type IpdKS = Knowledge * Map<Knowledge,float> //each indv has a primary ks and partial influece KSs
type W = Set<Id> * float
type Action = W list
type Payout = Action

type IpdState = {SumDiversity:float; NormalizedFit:float array; VMin:float; VMax:float}

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

let cooperation 
    st
    neighbor 
    indv =  
    let ids = set[indv.Id; neighbor.Id]
    let d = parmDiversity indv.Parms neighbor.Parms / st.SumDiversity  //normalize diversity
    let fNbr = st.NormalizedFit.[neighbor.Id]
    let fI = st.NormalizedFit.[indv.Id]
    let attraction = (fNbr - fI) // |> max 0.
//    if attraction < 0. then failwithf "attr neg %A" (f1,f1, attraction)
    //let coop = d * attraction
    let coop = if d <> 0. then attraction/ (d * 0.4) else System.Double.MaxValue
    //let coop = d * 0.5 * attraction
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
            fs |> List.sumBy snd |> max 1.0    //cap alt KS influence to 1.0
            )
        |> Map.ofList
    ksw

let removePrimaryKS ks m = Map.remove ks m

let createKs primary others = primary, removePrimaryKS primary others

let updateIndv (vmin,vmax) cmprtr (pop:Population<IpdKS>) indv payout =
    let payout = payout |> List.filter (fun (_,f) -> f < vmin)
    let vmx = payout |> List.filter (fun (_,f) -> f >= vmax)
    let indv = 
        match vmx.Length,payout.Length with
        | 0,0 -> {indv with KS=fst indv.KS,Map.empty}
        | 0,_ -> let ks = createKs (fst indv.KS) (updateKsw pop payout indv)
                 {indv with KS=ks}
        | 1,_ ->
            let nhbr = pop.[other indv.Id (fst vmx.[0])]
            let (ks,_):IpdKS = nhbr.KS
            //let ksw = updateKsw pop payout indv 
            {indv with KS = createKs ks Map.empty}
        | _,_ ->  
            let i = CAUtils.rnd.Value.Next(0,vmx.Length - 1)
            let nhbr = pop.[other indv.Id (fst vmx.[i])]
            let (ks,_):IpdKS = nhbr.KS
            //let ksw = updateKsw pop payout indv 
            {indv with KS = createKs ks Map.empty}  
    indv

let updatePop vmx cmprtr pop (payouts:Payout array) = 
    let pop = pop |> Array.Parallel.map(fun indv ->
        updateIndv vmx cmprtr pop indv payouts.[indv.Id]
    )
    pop

let createState (vmin,vmax) cmprtr pop =
    {
        SumDiversity = sampleAvgDiversity pop * float pop.Length
        NormalizedFit = normalizePopFitness (0., 1.0) cmprtr pop
        VMin = vmin
        VMax = vmax
    }

let rec outcome state cmprtr (pop,beliefSpace) (payouts:Payout array) =
    let vmx = (state.VMin, state.VMax)
    let pop = updatePop vmx cmprtr pop payouts
    let state = createState vmx cmprtr pop
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
    let state = createState (vmin,vmax) cmprtr pop
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
            let p = ksMap.[mainKs].Influence 0.7 p
            (p,otherKs) ||> Map.fold (fun p k w -> ksMap.[k].Influence w p))
    pop 

let knowledgeDist (vmin,vmax) comparator pop =
    let g = game (vmin,vmax)  comparator pop
    KDContinousStrategyGame.knowledgeDist comparator g

