module CARunner
open CA
open FSharp.Collections.ParallelSeq

///create the belief space structure that is normally used in CAs
let defaultBeliefSpace parmDefs minmax fitness =
    Roots [ 
        Node (SituationalKS.create parmDefs minmax 15,
            [
                Leaf (HistoricalKS.create parmDefs minmax 100)
                Leaf (DomainKS.create parmDefs minmax fitness)
            ])
        Leaf (NormativeKS.create parmDefs minmax)
        Leaf (TopographicKS.create parmDefs minmax fitness)
        ]

///evaluate the finess of the population
let evaluate fitness pop = 
    let pop =
        pop
        |> Array.Parallel.map (fun (ind:Individual<_>) -> 
            {ind with Fitness=fitness ind.Parms})
    pop

///default acceptance function used in most CAs
let acceptance topProportion minmax beliefSpace (pop:Population<_>) =
    let sign = if minmax 2. 1. then -1. else +1. 
    let take = (float pop.Length) * topProportion |> int
    let topInds = 
        pop 
        |> PSeq.sortBy (fun ind -> sign * ind.Fitness) 
        |> Seq.truncate take
        |> Seq.toArray
    topInds

///default belief space update function
let update envChanged beliefSpace bestInds  =          
    let rec update bestInds ksTree  =
        match ksTree with
        | Roots ksList                  ->  Roots (ksList |> List.map (update bestInds))
        | Node ({Accept=accept},ksList) ->
                                            let inds,ks = accept envChanged bestInds
                                            Node (ks,ksList |> List.map (update inds))
        | Leaf {Accept=accept}          -> 
                                            let _,ks = accept envChanged bestInds
                                            Leaf ks
    update bestInds beliefSpace

let detectChange (fitness:Fitness) (best:Marker list) =
    if not Settings.isDynamic then
      false
    elif List.isEmpty best then
      false
    else
      let b1 = best.[0]
      let f2 = fitness.Value b1.MParms
      abs (f2 - b1.MFitness) > 0.00001

///single step CA
let step envChanged {CA=ca; Best=best; Count=c; Progress=p} maxBest =
    let pop             = evaluate ca.Fitness.Value ca.Population
    let topInds         = ca.Acceptance ca.BeliefSpace pop
    let oldBest = 
        if envChanged then 
            match best with 
            | []   -> [] 
            | x::_ -> 
                let f = ca.Fitness.Value x.MParms //re-evaluate old best under new fitness landscape
                [{x with MFitness=f}]
        else 
            best
    let newBest = 
        match oldBest,topInds with
        | _ ,[||]   -> oldBest
        | [],is     -> [CAUtils.toMarker is.[0]]
        | b::_,is when ca.Comparator is.[0].Fitness b.MFitness -> (CAUtils.toMarker is.[0]::oldBest) |> List.truncate maxBest
        | _         -> oldBest
    let blSpc           = ca.Update envChanged ca.BeliefSpace topInds
    let fInfluence      = match ca.Influence with Influence(k) -> k
    let pop,blSpc,finf = fInfluence envChanged pop blSpc ca.Network ca.Fitness ca.Comparator 
    //let pop             = ca.Influence blSpc pop
    {
        CA =
            {ca with
                Population   = pop
                BeliefSpace  = blSpc
                Influence    = finf
            }
        Best = newBest
        Progress = newBest.[0].MFitness::p |> List.truncate 100
        Count = c + 1
    }

///run till termination
let run desc termination maxBest ca =
    let rec loop stp = 
        let stp = step false stp maxBest
        let best = if stp.Best.Length > 0 then stp.Best.[0].MFitness else 0.0
        match stp.Progress.Length with
        | 0 -> printfn "starting '%s'" desc
        | 1 -> printfn "'%s' %d %f %A" desc stp.Count stp.Progress.[0] stp.Best.[0].MParms
        | _ when stp.Progress.[0] = stp.Progress.[1] |> not -> 
            printfn "'%s' %d %f %A" desc stp.Count stp.Progress.[0] stp.Best.[0].MParms
        | _ -> ()
        if termination stp then
            stp
        else
            loop stp
    loop {CA=ca; Best=[]; Count=0; Progress=[]}


let ``terminate if no improvement in 5 generations`` (step:TimeStep<_>) =
    match step.Progress with
    | f1::f2::f3::f4::f5::_ when f1=f2 && f2=f3 && f3=f4 && f4=f5 -> true
    | _ -> false
        
        
