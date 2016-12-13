module KDWeightedMajority
open CA
open FSharp.Collections.ParallelSeq

let private wmDist pop network (ksMap:System.Collections.Generic.IDictionary<Knowledge,float>) ksWheel indv =
    let nhbrs = network pop indv.Id
    let directKS = Probability.spinWheel ksWheel  //wtd. prob. determined direct KS 
    let ksCounts = nhbrs |> Array.map (fun i -> i.KS,1) |> Array.append [|directKS,1|]
    let wtdKSCnts =    //weighted sum of ks counts for indv + neighbors
        ksCounts 
        |> Array.groupBy fst 
        |> Array.map (fun (ks,xs) -> ks,(xs |> Array.sumBy snd  |> float) * ksMap.[ks])
    let kd,_ = wtdKSCnts |> Array.maxBy snd
    let possibleConflicts = wtdKSCnts  |> Array.filter (fun (n,c)->n=kd)
    let kd = 
        if possibleConflicts.Length > 1 then
            let (kd,_) = possibleConflicts.[CAUtils.rnd.Value.Next(possibleConflicts.Length)]
            kd
        else
            kd
//    printfn "win kd %A" kd
    {indv with KS = kd}

let LOST_KS_WT = 0.1 // percentage of pop that is randomly assigned a KS that was pushed out

let rec kdLoop allKSSet every gen isBetter (pop,b) network =
    let nrmlzdFit = CAUtils.normalizePopFitness (0.,1.) isBetter pop
    let avgPopFit = Array.average nrmlzdFit
    let ksFit = pop |> PSeq.map(fun indv -> indv.KS, nrmlzdFit.[indv.Id]) |> PSeq.groupBy fst
    let ksFit = ksFit |> PSeq.map(fun (ks,kss) -> ks, kss |> Seq.sumBy snd) |> PSeq.toArray
    let ksWheel = Probability.createWheel ksFit                      // weighted spin wheel
    let ksMap = dict ksWheel //ks weight lookup dictionary
    let pop =
        if  gen % every = 0 then
            let pop = pop |> Array.Parallel.map (wmDist pop network ksMap ksWheel)
            let missingKS = pop |> Array.Parallel.map (fun i -> i.KS) |> set |> Set.difference allKSSet
            for ks in missingKS do
                for _ in 0 .. (float pop.Length * LOST_KS_WT |> int) do
                    let indx = CAUtils.rnd.Value.Next(pop.Length)
                    //mutate local copy of pop array
                    pop.[indx] <- {pop.[indx] with KS = ks}
            pop
        else
            pop

    pop,b,KD(kdLoop allKSSet every (gen + 1) isBetter)

let knowledgeDist allKSSet every isBetter =
    kdLoop allKSSet every 0 isBetter
