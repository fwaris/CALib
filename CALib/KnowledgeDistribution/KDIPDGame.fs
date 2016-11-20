module KDIPDGame
open CA
open KDContinousStrategyGame
open FSharp.Collections.ParallelSeq

type IpdKS = Map<Knowledge,float>
type W = Set<Id> * float
type Action = W list
type Payout = Action

let cooperation  neighbor indv =  set[indv.Id; neighbor.Id], 0.1 //tbd

let relativeCoop totalCoop (s,coop) = s,coop / totalCoop

let play _ indv neighbors payoff : Action =
    let coops = neighbors |> Seq.map (cooperation indv) |> Seq.toList
    let sumCoops = coops |> List.sumBy (fun (_,c) -> c)
    let ws = coops |> List.map (relativeCoop sumCoops)
    ws

let payoff _ indv indvActn (nhbrActns:Action seq) : Payout =
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

let VMAX = 1.5 //payout is between 0 and 2 
let VMIN = 0.5 

let updateKsw (pop:Population<_>) payout indv ksw =
    let fromRange = (0.,2.)
    let toRange = (0., 1.)
    let ksw = 
        payout 
        |> List.map (fun (idSet,f) ->  //set<id>, float [0-2]
            let nhbrId = other indv.Id idSet
            let ks = pop.[nhbrId].KS
            ks,f)
        |> List.groupBy fst
        |> List.map (fun (ks,fs)->
            ks,
            fs |> List.sumBy snd |> CAUtils.scaler fromRange toRange
            )
        |> Map.ofList
    ksw

let updateIndv cmprtr (pop:Population<_>) indv ksw payout =
    let payout = payout |> List.filter (fun (_,f) -> f < VMIN)
    let vmx = payout |> List.filter (fun (_,f) -> f >= VMAX )
    let indv,ksw = 
        match vmx.Length,payout.Length with
        | 0,0 -> indv, Map.empty
        | 0,_ -> indv, updateKsw pop payout indv ksw
        | 1,_ ->
            let nhbr = pop.[other indv.Id (fst vmx.[0])]
            {indv with KS = nhbr.KS}, updateKsw pop payout indv ksw  
        | _,_ ->  
            let i = CAUtils.rnd.Value.Next(0,vmx.Length - 1)
            let nhbr = pop.[other indv.Id (fst vmx.[i])]
            {indv with KS = nhbr.KS}, updateKsw pop payout indv ksw  
    ksw,indv

let updatePop cmprtr kswIn pop (payouts:Payout array) = 
    let kswOut = Array.copy kswIn
    let pop = pop |> Array.Parallel.mapi(fun i indv ->
        let ksw,indv = updateIndv cmprtr pop indv kswIn.[i] payouts.[i]
        kswOut.[i] <- ksw
        indv
    )
    pop,kswOut

let rec outcome ksWeights cmprtr (pop,beliefSpace) (payouts:Payout array) =
    let pop,ksWeights = updatePop cmprtr ksWeights pop payouts
    pop,
    beliefSpace,
    {
        Play = play
        Payoff = payoff
        Outcome = outcome ksWeights
    }

let initWeights pop = pop |> Array.Parallel.map (fun indv -> Map.ofList [indv.KS,1.0])

let init pop =
    let ksWeights = initWeights pop
    {
        Play = play
        Payoff = payoff
        Outcome = outcome ksWeights
    }
