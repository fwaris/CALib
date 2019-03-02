module KDStagHuntStatic
//knowledge distribution based on Stag Hunt game

open CA
open KDContinousStrategyGame

type ShKnowledge = Knowledge*int
type W = int*int*Knowledge*float
type Action = W seq //each player plays fitness
type Payout = Action

let primKS (k:ShKnowledge) = fst k

type ShState = 
    {
        FitnessAtInit   : float[]
        GensSinceInit   : int
        CoopGens        : int
        KSOrder         : Knowledge[]
        KSRange         : float*float
    }

let fitVal sign (_,_,_,f) = sign*f

//let defaultKSOrder = [|Topgraphical; Normative; Historical; Situational; Domain|]
let defaultKSOrder = [|Topgraphical; Domain; Topgraphical; Normative; Situational; Historical; Domain|]

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
      let scaledRank = CAUtils.scaler st.KSRange toRange (sign * indv.Fitness) //rank individual among its peers (neighbors)
      let newKS = st.KSOrder.[int scaledRank |> min (st.KSOrder.Length - 1)]
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

let influenceLevels =
    dict
        [
            Domain, 0.8 
            Normative, 1.0
            Situational, 1.0
            Historical, 1.0
        ]

let il ks = match influenceLevels.TryGetValue ks with true,v -> v | _ -> 1.0

let private shInfluence _ beliefSpace (pop:Population<ShKnowledge>) =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> dict
    let pop =
        pop
        |> Array.Parallel.map (fun p -> 
            let ks,i = p.KS
            let lvl = il ks 
            let lvl = 
              match ks with 
              | Domain -> 
                let l = lvl ** (float i) |> max 0.00001
                //printfn "dm lvl: %d %f" i l
                l
              | _ -> lvl
            let p = ksMap.[ks].Influence lvl  p
            p)
    pop 

let rec outcome state envCh optKind (pop,beliefSpace,_) (payouts:Payout array) =
    let cmp = CAUtils.mult optKind
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

let game ksOrder ksSet coopGens (pop:Population<ShKnowledge>) =
    
    let state = 
      {
        FitnessAtInit = pop |> Array.map (fun i-> i.Fitness)
        CoopGens = coopGens
        GensSinceInit = 0
        KSOrder = ksOrder |> Array.filter (fun k -> ksSet |> Set.contains k)
        KSRange = (0.0,defaultKSOrder.Length - 1|>float)
      }
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

let influence ksOrder coopGens beliefSpace pop =
    let ksOrder = ksOrder |> Option.defaultValue defaultKSOrder
    let ksSet = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type) |> set //use only KS used in belief space
    let g = game ksOrder ksSet coopGens pop
    KDContinousStrategyGame.influence g
