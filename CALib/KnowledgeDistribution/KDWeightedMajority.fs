module KDWeightedMajority
open CA
open FSharp.Collections.ParallelSeq
open System.Collections.Generic

let primKS (k:Knowledge) = k

let private wmDist pop network (ksMap:System.Collections.Generic.IDictionary<Knowledge,float>) ksWheel indv =
    let nhbrs = network pop indv.Id
    let directKS = Probability.spinWheel ksWheel  //wtd. prob. determined direct KS 
    let ksCounts = nhbrs |> Array.map (fun i -> i.KS,1) |> Array.append [|directKS,1|]
    let getWeight ks = match ksMap.TryGetValue ks with (true,x) -> x | _ -> 0.
    let wtdKSCnts =    //weighted sum of ks counts for indv + neighbors
        ksCounts 
        |> Array.groupBy fst 
        |> Array.map (fun (ks,xs) -> ks,(xs |> Array.sumBy snd  |> float) * getWeight ks)
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

let LOST_KS_WT = 0.20 // percentage of pop that is randomly assigned a KS that was pushed out

let influenceLevels =
    dict
        [
            Domain, 0.3
            Normative, 1.0
            Situational, 1.0
            Historical, 1.0
            Topgraphical, 1.0
        ]

let il ks = match influenceLevels.TryGetValue ks with true,v -> v | _ -> 1.0
 
let private wtdMajorityInfluence beliefSpace pop =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> dict
    let pop =
        pop
        |> Array.Parallel.map (fun p -> ksMap.[p.KS].Influence pop (il p.KS) p)
    pop

let rec kdLoop 
    allKSSet
    every 
    gen 
    envCh
    pop
    beliefSpace
    network
    fitness
    optKind
    =
    let nrmlzdFit = CAUtils.normalizePopFitness (0.,1.) (CAUtils.mult optKind) pop
    let avgPopFit = Array.average nrmlzdFit
    let ksFit = pop |> PSeq.map(fun indv -> indv.KS, nrmlzdFit.[indv.Id]) |> PSeq.groupBy fst
    let ksFit = ksFit |> PSeq.map(fun (ks,kss) -> ks, kss |> Seq.sumBy snd) |> PSeq.toArray
    let ws,ksWheel = Probability.createWheel ksFit                      // weighted spin wheel
    let ksMap = dict ws //ks weight lookup dictionary
    let pop =
        if  gen % every = 0 then
            let pop = pop |> Array.Parallel.map (wmDist pop network ksMap ksWheel)
            let missingKS = pop |> Array.Parallel.map (fun i -> i.KS) |> set |> Set.difference allKSSet
            for ks in missingKS do
                for _ in 1 .. (float pop.Length * LOST_KS_WT |> int) do
                    let indx = CAUtils.rnd.Value.Next(pop.Length)
                    //mutate local copy of pop array
                    pop.[indx] <- {pop.[indx] with KS = ks}
            pop
        else
            pop
    let pop = wtdMajorityInfluence beliefSpace pop
    pop,beliefSpace,Influence(kdLoop allKSSet every (gen + 1))

let influence beliefSpace every =
    let kss = CAUtils.flatten beliefSpace 
    let allKSSet = kss |> List.map (fun ks->ks.Type) |> set
    Influence(kdLoop allKSSet every 0)
