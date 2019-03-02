module Schelling
open CA
open KDContinousStrategyGame
open CAUtils

type SchKs = Knowledge 
type W = int*int*Knowledge*float
type Action = W seq
type Payout = Action
type ScState = {Indx:int}

let r1 th cmprtr (indv:Individual<SchKs>) (payout:Payout) : Individual<SchKs> =
    let sameKs = payout |> Seq.filter (fun (_,_,k,_) -> k = indv.KS)
    let poL = Seq.length payout
    let sksL = Seq.length sameKs
    let ratio = float  sksL / float poL
    if ratio > th then 
        indv // retain KS
    else
        let (_,_,newKs,_) = payout |> Seq.maxBy (fun (_,_,k,f)->cmprtr f)
        {indv with KS=newKs}

let updateIndv st rule cmprtr (pop:Population<SchKs>) (indv:Individual<SchKs>) payout =
    rule cmprtr indv payout
    
let updatePop st rule cmprtr (pop:Population<SchKs>) (payouts:Payout array) : Population<SchKs> = 
    let pop = pop |> Array.Parallel.map(fun indv ->
        updateIndv st rule cmprtr pop indv payouts.[indv.Id]
    )
    pop

let play state _ (indv:Individual<_>) neighbors payoff : Action = 
    neighbors |> Seq.map (fun p -> indv.Id,p.Id,indv.KS,indv.Fitness)

let payoff _ _ indv indvActn (nhbrActns:Action seq) : Payout = 
    let natcs = nhbrActns |> Seq.collect (fun x->x) |> Seq.filter (fun (_,nid,_,_) -> indv.Id=nid)
    seq {yield (indv.Id,indv.Id,indv.KS,indv.Fitness); yield! natcs}

let influenceLevels =
    dict
        [
            Domain, 0.3
            Normative, 1.0
            Situational, 1.0
            Historical, 1.0
        ]

let il ks = match influenceLevels.TryGetValue ks with true,v -> v | _ -> 1.0

let private schlInfluecne beliefSpace (pop:Population<SchKs>) =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> dict
    let pop =
        pop
        |> Array.Parallel.map (fun p -> 
            let lvl = il p.KS
            let p = ksMap.[p.KS].Influence lvl p
            p)
    pop 

let rec outcome state rule envCh optKind (pop,beliefSpace,_) (payouts:Payout array) =
    let cmp = CAUtils.comparator optKind
    let pop = updatePop state rule cmp pop payouts
    let pop = schlInfluecne beliefSpace pop
    pop,
    beliefSpace,
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state rule
    }

let game rule =
    let state = {Indx=0}
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state rule
    }

let influence rule =
    let g = game rule
    KDContinousStrategyGame.influence g


