module KDStackelberg

//knowledge distribution based on Stackelberg game

open CA
open KDContinousStrategyGame
open System.Collections.Generic

type StkKnowledge = Knowledge*int
type W = int*int*Knowledge*float //indvId, neighborId, Knowledge, fit
type Action = W seq //each player plays fitness
type Payout = Action

let MAX_RETAIN = 20

let primKS (k:StkKnowledge) = fst k
    
let updatePop sign (pop:Population<StkKnowledge>) (payouts:Payout array) : Population<StkKnowledge> = 

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

let play _ (indv:Individual<_>) neighbors payoff : Action = 
    neighbors |> Seq.map (fun p -> indv.Id,p.Id,fst indv.KS,indv.Fitness)

let payoff  _ indv indvActn (nhbrActns:Action seq) : Payout = 
    let natcs = nhbrActns |> Seq.collect (fun x->x) |> Seq.filter (fun (_,nid,_,_) -> indv.Id=nid)
    seq {yield (indv.Id,indv.Id,fst indv.KS,indv.Fitness); yield! natcs}

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
                //printfn "dm lvl: %d %f" i l
                l
              | _ -> lvl
            let p = ksMap.[ks].Influence lvl  p
            p)
    pop 

let rec outcome envCh cmprtr (pop,beliefSpace,_) (payouts:Payout array) =
    let cmp = if cmprtr 1. 0. then 1.0 else -1.
    let pop = updatePop cmp pop payouts
    let pop = stkInfluence beliefSpace pop
    pop,
    beliefSpace,
    {
        Play = play
        Payoff = payoff
        Outcome = outcome
    }
    
let game () =
    {
        Play = play
        Payoff = payoff
        Outcome = outcome 
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

let influence =
    let g = game ()
    KDContinousStrategyGame.influence g
