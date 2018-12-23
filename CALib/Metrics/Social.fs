module Social
open CA
open CAUtils
open FSharp.Collections.ParallelSeq

let ksNum = function
  | Situational   -> 1
  | Historical    -> 2
  | Normative     -> 3
  | Topgraphical  -> 4
  | Domain        -> 5
  | Other _       -> 6

let ksSegments = [1;2;3;4;5]

let rec nhbrTypes ca (seg:Individual<'a>->int) i ks  =
    seq {
        yield! ks
        if i > 0 then
            let nbrs = ks |> Array.collect (fun (id,_) -> 
                ca.Network ca.Population id |> Array.map (fun i -> i.Id,seg i))
            yield! nhbrTypes ca seg (i-1) nbrs
    }

let segregationAt radius prop segs ca seg indv =
      let nbrs = nhbrTypes ca seg radius [|indv.Id,seg indv|]
      let nbrsK = 
        nbrs
        |> set
        |> Seq.map (fun (id,k) -> k) 
        |> Seq.countBy yourself 
        |> Map.ofSeq
      let missingK = segs |> Seq.choose (fun k -> if nbrsK.ContainsKey k |> not then Some (k,0) else None) 
      let nbrsKComplete =
        nbrsK
        |> Map.toSeq
        |> Seq.append missingK
        |> Seq.map (fun (k,v)->k,float v)
      let totK = nbrsKComplete |> Seq.map snd |> Seq.sum
      let deviations_from_prop = nbrsKComplete |> Seq.map (fun (k,v) -> prop - (v / totK) |> abs) |> Seq.sum
      deviations_from_prop

let segregation radius prop segs ca seg =
    ca.Population 
    |> Array.Parallel.map (segregationAt radius prop segs ca seg)
    |> Array.average

let rmseDist (a:Individual<_>) (b:Individual<_>) = 
  Array.zip a.Parms b.Parms 
  |> Array.map (fun (xa,xb)-> (xa-xb)*(xa-xb)) 
  |> Array.sum
  |> fun x -> x / (float a.Parms.Length) 
  |> sqrt

let diffusionAt<'a> (ca:CA<'a>) (i:Individual<'a>) =
    let nbrs = ca.Network ca.Population i.Id
    let rmse = nbrs |> Array.map (rmseDist i) |> Array.sum
    let avgRmse = rmse / (float nbrs.Length)
    avgRmse

let diffusion  (ca:CA<'a>) = 
    ca.Population 
    |> Array.Parallel.map (diffusionAt ca)
    |> Array.average