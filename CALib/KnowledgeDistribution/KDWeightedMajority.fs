module KDWeightedMajority
open CA
open FSharp.Collections.ParallelSeq

let private wmDist pop network (nrmlzdFit:Map<_,_>) indv =
    let nhbrs = network pop indv.Id
    let allFrnds = Array.append [|indv|] nhbrs
    let kdCounts = allFrnds |> Array.countBy (fun i->i.KS)
    let totalKD = kdCounts |> Array.sumBy snd |> float
    let nrmlzdCnts = kdCounts |> Array.map (fun (k,v) -> k, float v / totalKD * nrmlzdFit.[k])
    let kd,_ = nrmlzdCnts |> Array.maxBy snd
    let possibleConflicts = nrmlzdCnts  |> Array.filter (fun (n,c)->n=kd)
    let kd = 
        if possibleConflicts.Length > 1 then
            let (kd,_) = possibleConflicts.[CAUtils.rnd.Value.Next(possibleConflicts.Length)]
            kd
        else
            kd
//    printfn "win kd %A" kd
    {indv with KS = kd}

let rec knowledgeDist every gen isBetter (pop,b) network =
    let nrmlzdFit = CAUtils.normalizePopFitness (0.,1.) isBetter pop
    let ksFit = pop |> PSeq.map(fun indv -> indv.KS, nrmlzdFit.[indv.Id]) |> PSeq.groupBy fst
    let ksFit = ksFit |> PSeq.map(fun (ks,kss) -> ks, kss |> Seq.sumBy snd) |> Map.ofSeq
    let ksTotal = (0.,ksFit) ||> Map.fold(fun acc k v -> acc + v)
    let nrmlzdKdFit = ksFit |> Map.map (fun k v -> v / ksTotal)
    let pop =
        if  gen % every = 0 then
            pop |> Array.Parallel.map (wmDist pop network nrmlzdKdFit)
        else
            pop
    pop,b,KD(knowledgeDist every (gen + 1) isBetter)
