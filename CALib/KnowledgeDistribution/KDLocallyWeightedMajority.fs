module KDLocallyWeightedMajority
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

let private localKSFitness isBetter (indvs:Individual<Knowledge> array) =
    let mc = maxConverter isBetter
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
let private locallyWeightedMajority comparator (indv,friends:Individual<Knowledge> array) = 
    let acc = Map.add indv.KS 1.
    let grp = Array.append [|indv|] friends
    let relFit = localKSFitness comparator grp
    let kdCounts = grp |> Array.countBy (fun i->i.KS)
    let totalKD = float kdCounts.Length
    let nrmlzdCnts = kdCounts |> Array.map (fun (k,v) -> k, float v / totalKD * relFit.[k])
    let kd,_ = nrmlzdCnts |> Array.maxBy snd
    {indv with KS = kd}

let rec knowledgeDist comparator (pop,b) network =
    let pop =
        pop
        |> PSeq.ordered
        |> PSeq.map (fun ind -> ind,network pop ind.Id)
        |> PSeq.map (locallyWeightedMajority comparator)
        |> PSeq.toArray
    pop,b,KD(knowledgeDist comparator)
