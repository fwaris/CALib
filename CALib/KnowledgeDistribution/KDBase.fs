module KDBase
open CA
open FSharp.Collections.ParallelSeq

///majority wheel KS distribution
let majority (ind,friends:Individual array) = 
    {ind with 
        KS = 
            let f = friends.[CAUtils.rnd.Value.Next(0,friends.Length-1)]
            if f.Fitness > ind.Fitness then 
                f.KS
            else
                ind.KS
    }

///weighted majority wheel KS distribution
let weightedMajority (ind,friends:Individual array) = 
    {ind with 
        KS = 
            let weighted = 
                friends 
                |> Seq.groupBy (fun i -> i.KS)                                        //group by KS
                |> Seq.map (fun (ks,inds) -> ks,inds |> Seq.sumBy (fun i->i.Fitness)) //sum fitness of each group 
                |> Seq.fold                                                           //calc low-high ranges for each group
                    (fun acc (ks,f) -> 
                        match acc with 
                        | []            -> (ks,(0.,f))::acc
                        | (_,(l,h))::_  -> (ks,(h,f+h))::acc
                    )
                    []
            let sum = snd <| snd weighted.Head 
            let  r = CAUtils.rnd.Value.NextDouble()  * sum
            let chosen = weighted |> List.rev |> List.pick (fun (ks,(l,h)) -> if r < h then Some ks else None)
            chosen
    }

///generic knowledge distribution
let knowledgeDistribution distributionType pop network =
    pop
    |> PSeq.map (fun ind -> ind,network pop ind.Id)
    |> PSeq.map distributionType
    |> PSeq.toArray

