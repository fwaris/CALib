module KDSimpleMajority
open CA
open FSharp.Collections.ParallelSeq

let smDist pop network indv =
    let nhbrs = network pop indv.Id
    let indvsPool = Array.append [|indv|] nhbrs
    let kdCounts = 
        indvsPool 
        |> Array.countBy (fun i->i.KS.MinimumElement) 
        |> Array.sortBy (fun (_,x) -> -x)
    let (maxKS,maxC) = kdCounts.[0]
    let cnddtKS = kdCounts |> Array.takeWhile (fun (x,c) -> c >= maxC)
    let slctdKS =
        if cnddtKS.Length = 1 then
            maxKS
        else
            fst cnddtKS.[CAUtils.rnd.Value.Next(0,cnddtKS.Length-1)]
    {indv with KS=set[slctdKS]}

let rec knowledgeDist (pop,b) network =
    let pop = 
        pop
        |> PSeq.ordered
        |> PSeq.map (smDist pop network)
        |> PSeq.toArray
    pop,b,KD(knowledgeDist)

