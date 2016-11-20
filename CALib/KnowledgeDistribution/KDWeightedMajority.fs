module KDWeightedMajority
open CA
open FSharp.Collections.ParallelSeq

let private maxConverter isBetter = 
    if isBetter 2. 1. then 
        fun x->x 
    else 
        fun x -> 
            if x <> 0. then
                1. / x
            else
                System.Double.MaxValue

let private totalKSFit isBetter (pop:Population<Knowledge>) =
    let mc = maxConverter isBetter
    pop 
    |> PSeq.map (fun i -> i.KS,i.Fitness) 
    |> PSeq.groupBy fst
    |> PSeq.map (fun (k,fs) -> 
        k, 
        (fs |> Seq.sumBy snd) / float (Seq.length fs) 
        |> mc
        )
    |> Map.ofSeq

let private wmDist pop network (nrmlzdFit:Map<_,_>) indv =
    let nhbrs = network pop indv.Id
    let acc = Map.add indv.KS 1.
    let ks = Array.append [|indv|] nhbrs
    let kdCounts = ks |> Array.countBy (fun i->i.KS)
    let totalKD = float kdCounts.Length
    let nrmlzdCnts = kdCounts |> Array.map (fun (k,v) -> k, float v / totalKD * nrmlzdFit.[k])
    let kd,_ = nrmlzdCnts |> Array.maxBy snd
    {indv with KS = kd}

let rec knowledgeDist isBetter (pop,b) network =
    let ksFit = totalKSFit isBetter pop
    let totalFit = (0.,ksFit) ||> Map.fold (fun acc _  v -> v + acc)
    let nrmlzdKdFit = ksFit |> Map.map (fun _ v -> v / totalFit)
    let pop = pop |> Array.Parallel.map (wmDist pop network nrmlzdKdFit)
    pop,b,KD(knowledgeDist isBetter)
