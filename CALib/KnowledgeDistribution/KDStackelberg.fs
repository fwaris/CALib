module KDStackelberg

//knowledge distribution based on Stackelberg game

open CA
open KDContinousStrategyGame
open System.Collections.Generic

type StkKnowledge = Knowledge*int
type W = int*int*Knowledge*float //indvId, neighborId, Knowledge, fit
type Action = W seq //each player plays fitness
type Payout = Action

let MAX_RETAIN = 100
let MOMEMTUM_CARRY = 100.0
let DIST_AFTER_GENS = 7

type ShState = 
    {
        Gen                 : int
        DistributeAfterGens : int
        PrevFit             : float[]
        Momentum            : float[]
        Sign                : float
    }

let primKS (k:StkKnowledge) = fst k
    
let updatePop state sign (pop:Population<StkKnowledge>) (payouts:Payout array) : Population<StkKnowledge> = 

  let rankedPop = pop |> Array.map (fun i -> i.Id, fst i.KS, i.Fitness * sign) |> Array.sortByDescending (fun (_,_,f)->f)

  let ksByFit = 
    rankedPop
    |> Array.map(fun (_,k,f) -> k,f)
    |> Array.groupBy fst
    |> Array.map (fun (k,xs) ->k, xs |> Array.map snd |> Array.sum )
    |> Array.sortByDescending snd
    |> Array.mapi (fun i (k,f) -> (i,k,f))
  
  let chunked = rankedPop |> Array.chunkBySize ksByFit.Length

  let ksAssigns = Array.zeroCreate pop.Length
  let exclusions = Dictionary<Knowledge,HashSet<int>>() // Knowldge (key) cannot be assigned to ids in hashset as its been assigned to a neighbor

  ksByFit |> Array.iter (fun (_,k,_) -> exclusions.Add(k,new HashSet<int>())) //init exclusions

  ksByFit 
  |> Array.iter (fun (i,k,_) -> 
    let chnk = chunked.[i] //look at chunks in order
    chnk |> Array.iter (fun (id,_,_) -> 
      if ksAssigns.[id] = None then
        let exSet = exclusions.[k]
        if exSet.Contains(id) then
          () //can't assign this KS as neighbor has it
        else
          if state.Momentum.[i] > 0.0 then
                () // keep the previous KS as the individual is improving under KS
          else
              ksAssigns.[id] <- Some k
              payouts.[id] |> Seq.iter (fun (_,nbrid,_,_) -> exSet.Add(nbrid) |> ignore) //exclude neighbors from getting this KS
    ))

  //randomly assign KS to any individual still left
  ksAssigns 
  |> Array.iteri (fun i k -> 
    if k = None then 
      let idx = Probability.RNG.Value.Next(ksByFit.Length)
      let (_,k,_) = ksByFit.[idx]
      ksAssigns.[i] <- Some k
    )

  //update pop with new Assignments
  pop |> Array.mapi (fun i indv ->
    let newKs = ksAssigns.[i].Value
    let currKs,c = indv.KS
    let c = if newKs = currKs then (c+1) |> min MAX_RETAIN else 0
    {indv with KS=newKs,c}
    )

let incrementRetainCount pop =
  //update pop with new Assignments
  pop |> Array.map (fun indv -> 
    let currKs,c = indv.KS
    let c = (c+1) |> min MAX_RETAIN 
    {indv with KS=currKs,c}
    )

let updateState state pop =

    let momentum = pop |> Array.mapi (fun i indv -> 
                let chng = if indv.Fitness * state.Sign > state.PrevFit.[i] * state.Sign then +1.0 else -1.0
                state.Momentum.[i] + chng)

    let prevFit = pop |> Array.map (fun i->i.Fitness)

    { state with
        Gen      = state.Gen + 1
        Momentum = momentum
        PrevFit = prevFit
    }

let play state _ (indv:Individual<_>) neighbors payoff : Action = 
    if state.Gen % state.DistributeAfterGens = 0 then
        neighbors |> Seq.map (fun p -> indv.Id,p.Id,fst indv.KS,indv.Fitness)
    else
        Seq.empty

let payoff state _ indv indvActn (nhbrActns:Action seq) : Payout = 
    if state.Gen % state.DistributeAfterGens = 0 then
        let natcs = nhbrActns |> Seq.collect (fun x->x) |> Seq.filter (fun (_,nid,_,_) -> indv.Id=nid)
        seq {yield (indv.Id,indv.Id,fst indv.KS,indv.Fitness); yield! natcs}
    else
        Seq.empty


let influenceLevels =
    dict
        [
            Domain, 0.8 
            Normative, 1.0
            Situational, 1.0
            Historical, 1.0
        ]

let il ks = match influenceLevels.TryGetValue ks with true,v -> v | _ -> 1.0

let private stkInfluence beliefSpace (pop:Population<StkKnowledge>) =
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
                //if i > 10 then
                //    printfn "dm lvl: %d %f" i l
                l
              | _ -> lvl
            let p = ksMap.[ks].Influence lvl  p
            p)
    pop 

let initState sign pop =
    {
        DistributeAfterGens = DIST_AFTER_GENS 
        PrevFit             = pop |> Array.map(fun i -> i.Fitness)
        Momentum            = pop |> Array.map (fun _ -> 0.0)
        Gen                 = 0
        Sign                = sign
    }

let resetState state pop = 
    {state with 
        Gen=0
        PrevFit = pop |> Array.map (fun i -> i.Fitness)
        Momentum = state.Momentum |> Array.map (fun m-> m |> min 0.0 |> max MOMEMTUM_CARRY) //keep some positve momentum around
    }

let reset state pop =
    let state = initState state.Sign pop
    let pop = pop |> Array.map(fun indv -> {indv with KS=fst indv.KS,0})
    state,pop

let rec outcome state envCh cmprtr (pop,beliefSpace,_) (payouts:Payout array) =
    let state,pop = if envCh then reset state pop else updateState state pop,pop
    let pop,state =
        if state.Gen % state.DistributeAfterGens = 0 then
            let cmp = if cmprtr 1. 0. then 1.0 else -1.
            updatePop state cmp pop payouts,
            resetState state pop
        else
            incrementRetainCount pop,
            state

    let pop = stkInfluence beliefSpace pop

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

let initKS (pop:Population<Knowledge>) : Population<StkKnowledge> = 
    pop 
    |> Array.Parallel.map (fun indv -> 
        {
            Id = indv.Id
            Fitness = indv.Fitness
            Parms = indv.Parms
            KS=indv.KS,0
        })


let influence cmprtr pop =
    let sign = if cmprtr 1.0 0.0 then +1.0 else -1.0
    let state = initState sign pop
    let g = game state
    KDContinousStrategyGame.influence g
