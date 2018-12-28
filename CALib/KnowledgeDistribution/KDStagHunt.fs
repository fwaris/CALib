module KDStagHunt
//knowledge distribution based on Stag Hunt game

open CA
open KDContinousStrategyGame
open MetaLrn
open System.Collections.Generic

let ksOrder1 = [|Topgraphical; Domain; Topgraphical; Normative; Situational; Historical; Domain|]
let ksOrder2 = [|Topgraphical; Topgraphical; Normative; Historical; Situational; Domain|]

type ShKnowledge = Knowledge*int
type W = int*int*Knowledge*float
type Action = W seq //each player plays fitness
type Payout = Action

let primKS (k:ShKnowledge) = fst k

type KSOrder = {Order:Knowledge[]; Range:float*float}

type ShState = 
    {
        FitnessAtInit   : float[]
        GensSinceInit   : int
        CoopGens        : int
        Schemes         : MLState<float>
        CurrOrder       : KSOrder
        CurrInfLvls     : IDictionary<Knowledge,float>
    }

let defaultInfluenceLevels =
        [
            Domain, 0.8 
            Normative, 1.0
            Situational, 1.0
            Historical, 1.0
        ]

let updateScheme state =
    let schem = MetaLrn.currentScheme state.Schemes 
    let lvls = defaultInfluenceLevels |> List.map (fun (k,i)->k,schem*i) |> dict
    { state with
        CurrInfLvls = lvls
    }

let initState cmprtr (ksSet:Set<Knowledge>) coopGens (pop:Population<ShKnowledge>) =
    let orders = [|ksOrder1; ksOrder2|] |> Array.map (Array.filter ksSet.Contains)
    let ksorders = orders |> Array.map (fun o -> {Order=o; Range=0.0, o.Length - 1 |> float})
    let policies = [| 0.75; 1.0; 1.25 |]
    let schemes = MetaLrn.initML cmprtr policies
    {
        FitnessAtInit = pop |> Array.map (fun i-> i.Fitness)
        CoopGens = coopGens
        GensSinceInit = 0
        Schemes = schemes
        CurrOrder = ksorders.[0]
        CurrInfLvls = dict defaultInfluenceLevels 
    }
    |> updateScheme

let fitVal sign (_,_,_,f) = sign*f

let private sqr x = x * x
let private std mean n xs = (xs |> Seq.map (fun x -> mean - x |> sqr) |> Seq.sum) / float n |> sqrt

let updateIndv_CoopGen  st sign (pop:Population<ShKnowledge>) (payouts:Payout[]) (indv:Individual<ShKnowledge>) = 
  let ind_pouts = payouts.[indv.Id]
  let maxFit = ind_pouts |> Seq.maxBy (fitVal sign)
  let minFit = ind_pouts |> Seq.minBy (fitVal sign)
  let toRange = fitVal sign minFit, fitVal sign maxFit
  let mn = fst toRange
  let mx = snd toRange
  if CAUtils.isValidNum mn && CAUtils.isValidNum mx && mn <> mx then
      let order = st.CurrOrder
      let scaledRank = CAUtils.scaler order.Range toRange (sign * indv.Fitness) //rank individual among its peers (neighbors)
      let newKS = order.Order.[int scaledRank |> min (order.Order.Length - 1)]
      let _,c = indv.KS
      {indv with KS=newKS,c+1}
  else
      indv

let updateIndv_EvalGen state sign (pop:Population<ShKnowledge>) (payouts:Payout[]) (indv:Individual<ShKnowledge>)= 
  if indv.Fitness * sign > state.FitnessAtInit.[indv.Id] * sign then
    //let ks,_ = indv.KS
    //{indv with KS=ks,0}
    indv
  else
    let dominantKS,_ = 
      payouts.[indv.Id]
      |> Seq.map(fun (_,_,k,f)->k,sign*f) 
      |> Seq.groupBy fst
      |> Seq.map (fun (k,xs)-> k,xs|>Seq.map snd |> Seq.sum)
      |> Seq.sortByDescending snd
      |> Seq.item 0
    {indv with KS=dominantKS,0}
    
let updatePop st cmprtr (pop:Population<ShKnowledge>) (payouts:Payout array) : Population<ShKnowledge> = 
    let isEvalGen = st.GensSinceInit = st.CoopGens 
    if isEvalGen then
      pop |> Array.Parallel.map (updateIndv_EvalGen st cmprtr pop payouts)
    else
      pop |> Array.Parallel.map (updateIndv_CoopGen st cmprtr pop payouts)

let play _ _ (indv:Individual<_>) neighbors payoff : Action = 
    neighbors |> Seq.map (fun p -> indv.Id,p.Id,fst indv.KS,indv.Fitness)

let payoff _ _ indv indvActn (nhbrActns:Action seq) : Payout = 
    let natcs = nhbrActns |> Seq.collect (fun x->x) |> Seq.filter (fun (_,nid,_,_) -> indv.Id=nid)
    seq {yield (indv.Id,indv.Id,fst indv.KS,indv.Fitness); yield! natcs}

let updateState state (pop:Population<ShKnowledge>) =
  let nextGen = state.GensSinceInit + 1
  if nextGen > state.CoopGens then
    {state with
      GensSinceInit = 0
      FitnessAtInit = pop |> Array.map (fun i->i.Fitness)
    }
  else
    {state with GensSinceInit = nextGen}

let il state ks = match state.CurrInfLvls.TryGetValue ks with true,v -> v | _ -> 1.0

let private shInfluence state beliefSpace (pop:Population<ShKnowledge>) =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> dict
    let pop =
        pop
        |> Array.Parallel.map (fun p -> 
            let ks,i = p.KS
            let lvl = il state ks 
            let lvl = 
              match ks with 
              | Domain -> 
                let l = lvl ** (float i) |> max 0.00001
                //if i > 10 then
                //    printfn "dm lvl: %d %f" i l
                l
              | _ -> lvl
            let p = ksMap.[ks].Influence lvl  p
            p)
    pop 

let resetCounts pop = pop |> Array.map(fun indv -> {indv with KS=fst indv.KS,0})

let rec outcome state envCh cmprtr (pop,beliefSpace,_) (payouts:Payout array) =
    let pop = if envCh then resetCounts pop else pop
    let state = 
        if envCh then 
            let schemes = MetaLrn.regimeChanged state.Schemes
            let curr = MetaLrn.currentScheme schemes
            printfn "scheme %d" schemes.Regimes.Head.Scheme
            { state with 
                Schemes = schemes
            }
            |> updateScheme
        else
            let schemes = MetaLrn.updateRegime state.Schemes pop
            { state with
                Schemes = schemes
            }
    let cmp = if cmprtr 1. 0. then 1.0 else -1.0
    let pop = updatePop state cmp pop payouts
    let pop = shInfluence state beliefSpace pop
    let state = updateState state pop
    pop,
    beliefSpace,
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state
    }

let game state =    
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state 
    }

let initKS (pop:Population<Knowledge>) : Population<ShKnowledge> = 
    pop 
    |> Array.Parallel.map (fun indv -> 
        {
            Id = indv.Id
            Fitness = indv.Fitness
            Parms = indv.Parms
            KS=indv.KS,0
        })

let influence cmprtr coopGens beliefSpace pop =
    let ksSet = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type) |> set //use only KS used in belief space
    let state = initState cmprtr ksSet coopGens pop
    let g = game state
    KDContinousStrategyGame.influence g
