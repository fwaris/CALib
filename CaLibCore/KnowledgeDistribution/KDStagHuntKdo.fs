module KDStagHunt_
//knowledge distribution based on Stag Hunt game

open CA
open KDContinousStrategyGame
open System.Collections.Generic
open System.Runtime.InteropServices
open Microsoft.ML

let ksOrder1 = [|Topgraphical; Domain; Topgraphical; Normative; Situational; Historical; Domain|]
let ksOrder2 = [|Topgraphical; Topgraphical; Normative; Historical; Situational; Domain|]

type ShKnowledge = Knowledge*int
type W = int*int*Knowledge*float
type Action = W seq //each player plays fitness
type Payout = Action
type Policy = {Start:float; End:float}

let primKS (k:ShKnowledge) = fst k

type KSOrder = {Order:Knowledge[]; Range:float*float}
type Parms = {Start:float; End:float; Decay:float; Result:IRunResult list; ParmSet:ParameterSet}

type EnvPerf = {GensToBest:int; RegimeGens:int; HithertoBest:float; RegimeBest:float}

type ShState<'a> = 
    {
        FitnessAtInit   : float[]
        GensSinceInit   : int
        CoopGens        : int
        Parms           : Parms
        CurrOrder       : KSOrder
        CurrentTmprtr   : float
        CurrInfLvls     : IDictionary<Knowledge,float>
        NormdBest       : float
        GensSinceBest   : int
        Sign            : float
        EnvPerf         : EnvPerf
    }

let defaultInfluenceLevels =
        [
            Domain, 0.8 
            Normative, 1.0
            Situational, 1.0
            Historical, 1.0
        ]


let setLevels tmprtr state  = 
    let lvls = defaultInfluenceLevels |> List.map (fun (k,i)->k,tmprtr*i) |> dict   
    { state with
        CurrentTmprtr = tmprtr
        CurrInfLvls = lvls
    }

let propose prev = 
    let pset  = MetaLrnKdo.sweeper.ProposeSweeps(1,prev).[0]
    let _start = pset.[MetaLrnKdo.p_start].ValueText |> float
    let _end   = pset.[MetaLrnKdo.p_end].ValueText |> float
    let _decay = pset.[MetaLrnKdo.p_decay].ValueText |> float
    printfn "s=%f, e=%f, d=%f, m=%f" _start _end _decay (prev|> List.tryHead |> Option.map(fun x->x.MetricValue :?> float ) |> Option.defaultValue 0.0)
    {Start=_start; End=_end; Decay=_decay; Result=[]; ParmSet=pset}

let proposeScheme state =
    let prevR = state.Parms.Result |> List.tryHead |> Option.map(fun x->x.MetricValue :?> float) |> Option.defaultValue 0.0
    //MetaLrnKdo.Dbg.log state.Parms.Start state.Parms.End state.Parms.Decay  prevR
    let parms = propose state.Parms.Result
    { state with
        Parms = parms
    }
    |> setLevels parms.Start

let updateLevels state =
    let tmprtr = state.CurrentTmprtr * state.Parms.Decay |> max state.Parms.End
    //printfn "curr level %A" tmprtr
    state |> setLevels tmprtr

let STAGNATION_GENS = 30

let updateForStagnation state =
    if state.GensSinceBest > STAGNATION_GENS  && state.CurrentTmprtr = state.Parms.End then 
        let tmprtr = state.Parms.Start
        { state with GensSinceBest = 0 } |> setLevels tmprtr
    else
        state

let initState cmprtr (ksSet:Set<Knowledge>) coopGens (pop:Population<ShKnowledge>) =
    let orders = [|ksOrder1; ksOrder2|] |> Array.map (Array.filter ksSet.Contains)
    let ksorders = orders |> Array.map (fun o -> {Order=o; Range=0.0, o.Length - 1 |> float})
    let sign = if cmprtr 1.0 0.0 then 1.0 else -1.0
    {
        FitnessAtInit = pop |> Array.map (fun i-> i.Fitness)
        CoopGens = coopGens
        GensSinceInit = 0
        Parms = propose []
        CurrOrder = ksorders.[0]
        CurrInfLvls = dict defaultInfluenceLevels 
        CurrentTmprtr = 1.0
        NormdBest = System.Double.MinValue 
        Sign = sign
        GensSinceBest = 0
        EnvPerf = {GensToBest=0; RegimeGens=0; HithertoBest=System.Double.MinValue; RegimeBest=System.Double.MinValue}
    }

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

let updateCoopGens state (pop:Population<ShKnowledge>) =
  let nextGen = state.GensSinceInit + 1
  if nextGen > state.CoopGens then
    let best = pop |> Array.map (fun i->i.Fitness * state.Sign) |> Array.max
    let newBest, genSince = if best > state.NormdBest then best,0 else state.NormdBest,state.GensSinceBest+1
    {state with
        GensSinceInit = 0
        FitnessAtInit = pop |> Array.map (fun i->i.Fitness)
        NormdBest = newBest
        GensSinceBest = genSince
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

let updateBest pop state  = 
    let regimeBest = pop |> Array.map (fun (p:Individual<_>) ->p.Fitness*state.Sign) |> Array.max
    let rgens = state.EnvPerf.RegimeGens+1
    let newBest,gensBest = if state.EnvPerf.RegimeBest < regimeBest then regimeBest,rgens else state.EnvPerf.RegimeBest,state.EnvPerf.GensToBest
    let hbest = state.EnvPerf.HithertoBest |> max regimeBest
    let envperf = {RegimeBest=newBest; GensToBest=gensBest; RegimeGens=rgens; HithertoBest=hbest}
    {state with EnvPerf=envperf}

let updateRegimeChange state =
    let result = state.EnvPerf.RegimeBest/state.EnvPerf.HithertoBest //* ( 1.0 - (float state.EnvPerf.GensToBest / float state.EnvPerf.RegimeGens))
    //let result = state.EnvPerf.RegimeBest/state.EnvPerf.HithertoBest * ( 1.0 - (float state.EnvPerf.GensToBest / float state.EnvPerf.RegimeGens))
    let envPerf = {state.EnvPerf with GensToBest=0; RegimeGens=0; RegimeBest=System.Double.MinValue}
    let parms = {state.Parms with Result=[RunResult(state.Parms.ParmSet,result,true)]}
    { state with
        Parms   = parms
        EnvPerf = envPerf
    }

let rec outcome state envCh cmprtr (pop,beliefSpace,_) (payouts:Payout array) =

    let pop = 
        match envCh with
        | Adjust -> resetCounts pop
        | _      -> pop

    let state = 
        match envCh with
        | Adjust | Track ->
            state |> updateRegimeChange |> proposeScheme 
        | NoChange ->
            state |> updateBest pop |> updateLevels |> updateForStagnation

    let pop = updatePop state state.Sign pop payouts
    //printfn "%A" state.CurrentTmprtr
    let pop = shInfluence state beliefSpace pop
    let state = updateCoopGens state pop 

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
