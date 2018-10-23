module Community
open TracingGame
open CAUtils
open System
open KDIPDGame
open CA
open System.Collections.Generic
open System.Text
open System.IO

let gamePrimKs = (fun ((k,_):IpdKS) ->k.KS)
let basePrimKs = (fun (k:Knowledge) -> k)
let inline fstPrimKs  (k:Knowledge,_) = k

type Cluster = {Members:Set<Id>; Type:int}

[<AutoOpen>]
module Smap =
  type Smap<'a> when 'a : comparison = {Map:Map<'a,int>; Ordered:'a list}
  let empty = {Map=Map.empty; Ordered=[]}
  let add a sm  = 
    let c = sm.Map.Count
    let m = sm.Map |> Map.add a c
    {Map=m;Ordered=a::sm.Ordered}


///clusters based on link strength between network individuals (game based)
let cluster  popSz vmin (wtdLinks:(Link list)[]) =
  let links =  wtdLinks |> Seq.collect (fun ls -> ls |> Seq.filter (fun (s,c)->c>vmin) |> Seq.map fst)
  let baskets = Array.create popSz Set.empty
  links |> Seq.iter (fun s -> 
    let i1 = s.MinimumElement
    let i2 = s.MaximumElement
    baskets.[i1] <- baskets.[i1].Add i2
    baskets.[i2] <- baskets.[i2].Add i1)
  let transactions = baskets |> Seq.mapi (fun i s -> i,s)
  let tree = FPGrowth.makeTree transactions
  let support = 3 
  let itemCount = 3
  let itemSets = FPGrowth.mineTree support tree 
  let largeItemSets = itemSets |> Seq.filter (fun (a,_) -> a.Count >= itemCount) |> Seq.map fst
  let clusters = largeItemSets |> Seq.map(fun is -> 
    let isCount = Set.count is
    let matchTxns = 
      baskets 
      |> Array.mapi (fun i s -> let s' = Set.intersect s is in if Set.count s' = isCount then Some i else None) 
      |> Array.choose yourself
    is,matchTxns)
  clusters //first part is supported items set individuals and 2nd part is individuals point to the itemset individuals
  //printfn "%A" clusters
  //()

//clusters based on knowledge sources of surrounding neighbors
let clusterKS<'k> (primKS:'k->Knowledge) (network:Network<'k>) (pop:CA.Population<'k>)  =

  let transactions = 
    pop 
    |> Array.map (fun p ->
      let ns = network pop p.Id
      let txns = 
        Array.append ns [|p|] 
        |> Array.map (fun i -> primKS i.KS) |> Array.countBy yourself |> set
      p.Id,txns
    )

  let tree = FPGrowth.makeTree transactions
  let support = (float pop.Length  * 0.1 |> int)
  let itemCount = 3
  let itemSets = FPGrowth.mineTree support tree |> Seq.filter (fun (s,c)->s|>Seq.sumBy snd >= itemCount)
  let n = 1
  let topN = itemSets |> Seq.sortByDescending snd |> Seq.truncate n |> Seq.map fst |> Seq.toArray
  transactions,topN

let clusterKSMembers<'k> (primKS:'k->Knowledge) (network:Network<'k>) (pop:CA.Population<'k>)  clusterTypes =
  let transactions,topN =  clusterKS<'k> (primKS:'k->Knowledge) (network:Network<'k>) (pop:CA.Population<'k>)
  let matched = 
    transactions 
    |> Array.choose (fun (id,ks) ->
      let bestMatches =  
        topN 
        |> Array.choose (fun top2s -> if Set.intersect ks top2s |> Set.count > 0 then Some top2s else None) 
        |> Array.map(fun ks -> ks, ks |> Seq.map snd |> Seq.sum)
        |> Array.sortByDescending snd
      if bestMatches.Length = 0 then 
        None 
      else 
        let bestMtch = bestMatches.[0] |> fst 
        let ksSet = bestMtch |> Set.map fst
        Some(id,bestMtch,ksSet))

  let clusters = 
    let ct = (!clusterTypes, matched) ||> Seq.fold (fun acc (_,s,_) -> if acc.Map |> Map.containsKey s then acc else acc |> Smap.add s) 
    clusterTypes := ct
    matched 
    |> Array.map (fun  (id,clusterType,ksSet) -> 
      let clusterMembers =  
        network pop id 
        |> Seq.append [pop.[id]] 
        |> Seq.filter (fun i -> ksSet.Contains (primKS i.KS)) 
        |> Seq.map (fun x->x.Id)
        |> set
      {Members=clusterMembers;Type=ct.Map.[clusterType]}
    )
  clusters

let ksMap = 
  dict
        [
            Domain      , "D"
            Historical  , "H"
            Situational , "S"
            Normative   , "N"
            Topgraphical, "T"
        ]

let ksSetString ks =  
  let sb = (StringBuilder(),ks) ||> Seq.fold(fun sb (k,c) -> sb.Append(ksMap.[k]).Append(c:int))
  sb.ToString()

let logCluster (str:StreamWriter) (id:string) (a:float) (i:int) (lndscp:int) primKs network pop =
  let _,topN = clusterKS primKs network pop
  let ks1 = if topN.Length > 0 then ksSetString topN.[0] else "N"
  str.Write id
  str.Write "\t"
  str.Write a
  str.Write "\t"
  str.Write i
  str.Write "\t"
  str.Write lndscp
  str.Write "\t"
  str.Write ks1
  str.Write "\t"
  str.WriteLine()

let initLog file = new StreamWriter(file:string)
