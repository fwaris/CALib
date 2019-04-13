module KDIPD
//knowledge distribution based on Stag Hunt game

open CA
open KDContinousStrategyGame
type Decision = Cooperate | Defect

let ALPHA = 1.5

type IpKnowledge = Knowledge*int
type W = int*int*Knowledge*float
type Action = W seq //each player plays fitness
type Payout = Action

let primKS (k:IpKnowledge) = fst k

type IpState = 
    {
        PrevFitness     : float[]
        Gens            : int
        KSOrder         : (Knowledge*int)[]
        KSRange         : float*float
        Sign            : float
    }

let fitVal sign (_,_,_,f) = sign*f

let defaultKSOrder = [|Topgraphical,0; Domain,0; Topgraphical,1; Normative,0; Situational,0; Historical,0; Domain,2|]

let updateIndv_Cooperation st (pop:Population<IpKnowledge>) (payouts:Payout[]) (indv:Individual<IpKnowledge>) = 
  let sign = st.Sign
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
      {indv with KS=newKS}
  else
      indv

let updateIndv_Defection state (pop:Population<IpKnowledge>) (payouts:Payout[]) (indv:Individual<IpKnowledge>) = 
  let ks,c = indv.KS
  {indv with KS=ks,c+1}

let decision st payout =
  let defections = payout |> Seq.sumBy(fun (_,id,_,f) -> if  st.Sign * f > st.PrevFitness.[id] then ALPHA else 0.0) 
  let defection_award = defections / (Seq.length payout |> float)
  if defection_award > 1.0 then Defect else Cooperate
    
let updatePop st (pop:Population<IpKnowledge>) (payouts:Payout array) : Population<IpKnowledge> = 
    pop |> Array.Parallel.map (fun indv ->
        match decision st payouts.[indv.Id] with
        | Cooperate -> updateIndv_Cooperation st pop payouts indv
        | Defect -> updateIndv_Defection st pop payouts indv)

let play _ _ (indv:Individual<_>) neighbors payoff : Action = 
    neighbors |> Seq.map (fun p -> indv.Id,p.Id,fst indv.KS,indv.Fitness)

let payoff _ _ indv indvActn (nhbrActns:Action seq) : Payout = 
    let natcs = nhbrActns |> Seq.collect (fun x->x) |> Seq.filter (fun (_,nid,_,_) -> indv.Id=nid)
    seq {yield (indv.Id,indv.Id,fst indv.KS,indv.Fitness); yield! natcs}

let updateState state (pop:Population<IpKnowledge>) =
    {state with
      Gens = state.Gens+1
      PrevFitness = pop |> Array.map (fun i->i.Fitness * state.Sign)
    }

let influenceLevels =
    dict
        [
            Domain, 0.8 
            Normative, 1.0
            Situational, 1.0
            Historical, 1.0
        ]

let il ks = match influenceLevels.TryGetValue ks with true,v -> v | _ -> 1.0

let private shInfluence _ beliefSpace (pop:Population<IpKnowledge>) =
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
    let pop = updatePop state pop payouts
    let pop = shInfluence state beliefSpace pop
    let state = updateState state pop
    pop,
    beliefSpace,
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state
    }

let game sign ksOrder ksSet (pop:Population<IpKnowledge>) =
    
    let state = 
      {
        PrevFitness = pop |> Array.map (fun i-> i.Fitness)
        Gens = 0
        KSOrder = ksOrder |> Array.filter (fun (k,_) -> ksSet |> Set.contains k)
        KSRange = (0.0,defaultKSOrder.Length - 1|>float)
        Sign = sign
      }
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state 
    }

let initKS (pop:Population<Knowledge>) : Population<IpKnowledge> = 
    pop 
    |> Array.Parallel.map (fun indv -> 
        {
            Id = indv.Id
            Fitness = indv.Fitness
            Parms = indv.Parms
            KS=indv.KS,0
        })

let influence optKind ksOrder beliefSpace pop =
    let ksOrder = ksOrder |> Option.defaultValue defaultKSOrder
    let ksSet = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type) |> set //use only KS used in belief space
    let g = game (CAUtils.mult optKind) ksOrder ksSet  pop
    KDContinousStrategyGame.influence g

