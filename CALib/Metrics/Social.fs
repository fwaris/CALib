module Social
open CA
open CAUtils

let ksNum = function
  | Situational   -> 1
  | Historical    -> 2
  | Normative     -> 3
  | Topgraphical  -> 4
  | Domain        -> 5
  | Other _       -> 6

let ksSegments = [1;2;3;4;5]

let baseSeg (indv:Individual<Knowledge>) = ksNum indv.KS

let segregation radius prop segs (ca:CA<'a>) (seg:Individual<'a>->int) =
  let rec clct i ks =
    seq {
      yield! ks
      if i > 0 then
          let nbrs = ks |> Array.collect (fun (id,_) -> 
            ca.Network ca.Population id |> Array.map (fun i -> i.Id,seg i))
          yield! clct (i-1) nbrs
      }

  ca.Population 
  |> Array.map (fun indv ->
    let nbrs = clct radius [|indv.Id,seg indv|]
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
  )
  |> Array.average