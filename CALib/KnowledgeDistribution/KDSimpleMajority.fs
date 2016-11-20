module KDSimpleMajority
open CA
open FSharp.Collections.ParallelSeq

let private smDist pop network (indv:Individual<Knowledge>) =
    let nhbrs = network pop indv.Id
    let indvsPool = Array.append [|indv|] nhbrs
    let kdCounts = 
        indvsPool 
        |> Array.countBy (fun i->i.KS) 
        |> Array.sortBy (fun (_,x) -> -x)
    let (maxKS,maxC) = kdCounts.[0]
    let cnddtKS = kdCounts |> Array.takeWhile (fun (x,c) -> c >= maxC)
    let slctdKS =
        if cnddtKS.Length = 1 then
            maxKS
        else
            fst cnddtKS.[CAUtils.rnd.Value.Next(0,cnddtKS.Length-1)]
    {indv with KS=slctdKS}

let rec knowledgeDist (pop,b) network =
    let pop = 
        pop
        |> Array.Parallel.map (smDist pop network)
    pop,b,KD(knowledgeDist)

