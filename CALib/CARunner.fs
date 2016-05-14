module CARunner
open CA
open FSharp.Collections.ParallelSeq

///create the belief space structure that is normally used in CAs
let defaultBeliefSpace parms minmax fitness =
    Roots [ 
        Node (SituationalKS.create minmax 2,
            [
                Leaf (HistoricalKS.create minmax 20)
                Leaf (DomainKS.create minmax fitness 2)
            ])
        Leaf (NormativeKS.create parms minmax)
        ]

///evaluate the finess of the population
let evaluate fitness = 
    PSeq.map (fun (ind:Individual) -> {ind with Fitness=fitness ind.Parms})
    >> PSeq.toArray

///default acceptance function used in most CAs
let acceptance take minmax beliefSpace (pop:Population) =
    let sign = if minmax 2. 1. then -1. else +1. 
    let topInds = 
        pop 
        |> PSeq.sortBy (fun ind -> sign * ind.Fitness) 
        |> PSeq.truncate take
        |> PSeq.toArray
    topInds

///default belief space update function
let update beliefSpace bestInds = 
    let rec update bestInds ksTree  =
        match ksTree with
        | Roots ksList                  ->  Roots (ksList |> List.map (update bestInds))
        | Node ({Accept=accept},ksList) ->
                                            let inds,ks = accept bestInds
                                            Node (ks,ksList |> List.map (update inds))
        | Leaf {Accept=accept}          -> 
                                            let _,ks = accept bestInds
                                            Leaf ks
    update bestInds beliefSpace

///default population influence function
let influence beliefSpace pop =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> Map.ofList
    pop
    |> PSeq.map (fun p -> ksMap.[p.KS].Influence p)
    |> PSeq.toArray


///single step CA
let step {CA=ca; Best=best; Count=c; Progress=p} maxBest =
    let pop         = evaluate ca.Fitness ca.Population
    let topInds     = ca.Acceptance ca.BeliefSpace pop
    let beliefSpace = ca.Update ca.BeliefSpace topInds
    let pop         = ca.KnowlegeDistribution pop ca.Network
    let pop         = ca.Influence beliefSpace pop
    let newBest = 
        match best,topInds with
        | _ ,[||]   -> best
        | [],is     -> [is.[0]]
        | b::_,is when ca.Comparator is.[0].Fitness b.Fitness -> (is.[0]::best) |> List.truncate maxBest
        | _         -> best
    {
        CA =
            {ca with
                Population  = pop
                BeliefSpace = beliefSpace
            }
        Best = newBest
        Progress = newBest.[0].Fitness::p |> List.truncate 100
        Count = c + 1
    }

///run till termination
let run termination maxBest ca =
    let rec loop stp = 
        let stp = step stp maxBest
        let best = if stp.Best.Length > 0 then stp.Best.[0].Fitness else 0.0
        printfn "step %i. fitness=%A" stp.Count best
        printfn "KS = %A" (stp.CA.Population |> Seq.countBy (fun x->x.KS))
        if termination stp then
            stp
        else
            loop stp
    loop {CA=ca; Best=[]; Count=0; Progress=[]}


let ``terminate if no improvement in 5 generations`` (step:TimeStep) =
    match step.Progress with
    | f1::f2::f3::f4::f5::_ when f1=f2 && f2=f3 && f3=f4 && f4=f5 -> true
    | _ -> false
        
        
