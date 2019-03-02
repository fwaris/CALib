module KDLocallyWeightedMajority
open CA
open FSharp.Collections.ParallelSeq

let private maxConverter optKind = 
    match optKind with
    | Maximize ->  fun x->x 
    | Minimize ->
        fun x -> 
            if x <> 0. then
                1. / x
            else
                System.Double.MaxValue

let private localKSFitness optKind (indvs:Individual<Knowledge> array) =
    let mc = maxConverter optKind
    indvs 
    |> Array.map (fun i -> i.KS,i.Fitness) 
    |> Array.groupBy fst
    |> Array.map (fun (k,fs) -> 
        k, 
        (fs |> Seq.sumBy snd) / float (Seq.length fs) 
        |> mc
        )
    |> Map.ofSeq

///weighted majority wheel KS distribution
let private locallyWeightedMajority optKind (indv,friends:Individual<Knowledge> array) = 
    let acc = Map.add indv.KS 1.
    let grp = Array.append [|indv|] friends
    let relFit = localKSFitness optKind grp
    let kdCounts = grp |> Array.countBy (fun i->i.KS)
    let totalKD = float kdCounts.Length
    let nrmlzdCnts = kdCounts |> Array.map (fun (k,v) -> k, float v / totalKD * relFit.[k])
    let kd,_ = nrmlzdCnts |> Array.maxBy snd
    {indv with KS = kd}

let rec influence
    envCh
    pop
    beliefSpace
    network
    fitness
    optKind
    =
    let pop =
        pop
        |> Array.Parallel.map (fun ind -> 
            (ind,network pop ind.Id) 
            |> locallyWeightedMajority optKind)
    let pop = CAUtils.defaultInfluence beliefSpace pop   
    pop,beliefSpace,Influence(influence)
