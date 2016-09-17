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
    let uadjWts = 
        friends 
        |> Seq.groupBy (fun i -> i.KS)                                        //group by KS
        |> Seq.map (fun (ks,inds) -> ks,inds |> Seq.sumBy (fun i->i.Fitness)) //sum fitness of each group 
    let sumWt = uadjWts |> Seq.sumBy snd
    if sumWt = 0. then 
        majority (ind,friends)                          //degenerate case, use simple majority
    else
        let weighted = 
            uadjWts 
            |> Seq.map (fun (k,w) -> k,w/sumWt)          //normalize each weight by total weight
            |> Seq.sortBy snd                            //sort in order of weights
            |> Seq.fold (fun (s,xs) (k,w) -> s+w,(k,s+w)::xs) (0.,[])    //cumulative weights
            |> snd
            |> List.rev 
        let  r = CAUtils.rnd.Value.NextDouble() 
        let chosen = weighted |>  List.pick (fun (ks,w) -> 
            if r < w then 
                Some ks 
            else 
                None)
        {ind with KS = chosen}

///generic knowledge distribution
let rec knowledgeDistribution distributionType (pop,b) network =
    let pop =
        pop
        |> PSeq.map (fun ind -> ind,network pop ind.Id)
        |> PSeq.map distributionType
        |> PSeq.toArray
    pop,b,KD(knowledgeDistribution distributionType)
