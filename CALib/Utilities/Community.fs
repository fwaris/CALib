///Community detection in CA population networks
///along with support for video generation of community formation
///in population spaces
module Community
open TracingGame
open CAUtils
open VizUtils
open System
open OpenCvSharp
open CA
open System.Collections.Generic
open System.Text
open System.IO

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

let qColors = 
  [|
    (166,206,227)
    (31,120,180)
    (178,223,138)
    (51,160,44)
    (251,154,153)
    (227,26,28)
    (253,191,111)
    (255,127,0)
    (106,61,154)
    (255,255,153)
    (177,89,40)
    50,220,181
    100,210,185
    238,190,170
    76,80,140
    189,07,107
    255,88,0
  |]
  |> Array.map (fun (b,g,r) -> Scalar(float b,float g,float r, 0.001))

let communityColors = 
  [|
        255,0,0
        50,50,255
        50,255,50
        250,250,50
        126,239,213
        50,228,181
        100,218,185
        238,232,170
        76,230,140
        189,183,107
        255,215,0
        255,5,0
        50,55,255
        50,5,50
        250,245,50
        126,180,213
        50,220,181
        100,210,185
        238,190,170
        76,80,140
        189,07,107
        255,88,0
   |]
   |> Array.map (fun (b,g,r) -> Scalar(float b,float g,float r, 1.0))


let drawClusters width (network:CA.Network<_>) (pop:CA.Population<_>) (mat:Mat) (clusters:Cluster[])=
    let rowCount = sqrt (float pop.Length)
    let halfRc = rowCount / 2.0 |> ceil
    let rowLen = int rowCount
    let w = float width
    let margin = (w / rowCount) |> ceil
    //printfn "margin = %f" margin
    let cl = w - 2. * margin
    let incr = cl / rowCount 
    let halfIncr = incr / 4.
    //printfn "incr %f, half %f " incr halfIncr
    let rad = incr/3.0 |> ceil |> int
    let draw1 i clr = 
        let r1 = i / rowLen 
        let c1 = i % rowLen
        let shiftY1 = if c1 % 2 = 0 then halfIncr else -halfIncr
        let y1 = margin + (float r1 * incr + shiftY1)
        let x1 = margin + (float c1 * incr)
        let ctr1 = Point(int x1, int y1)
        Cv2.Circle(!> mat,ctr1,rad+1,clr,25)
    let draw2 i j clr = 
        let r1 = i / rowLen 
        let c1 = i % rowLen
        let shiftY1 = if c1 % 2 = 0 then halfIncr else -halfIncr
        let y1 = margin + (float r1 * incr + shiftY1)
        let x1 = margin + (float c1 * incr)
        let ctr1 = Point(int x1, int y1)
        let r2 = j / rowLen 
        let c2 = j % rowLen
        let shiftY2 = if c2 % 2 = 0 then halfIncr else -halfIncr
        let y2 = margin + (float r2 * incr + shiftY2)
        let x2 = margin + (float c2 * incr)
        let ctr2 = Point(int x2, int y2)
        Cv2.Line(!> mat, ctr1,ctr2,clr,2)
        //Cv2.Circle(!> mat,ctr1,rad+1,clr,20, LineTypes.Link8)
   //pop |> Array.iter(fun p->draw p.Id brgColors.[2])
    clusters |> Seq.iter(fun cluster ->
      let clr  = qColors.[cluster.Type % qColors.Length]
      cluster.Members |> Set.iter(fun i -> draw1 i clr)
    )

let clusterColor i = qColors.[i % qColors.Length]

let ksSetString ks =  
  let sb = (StringBuilder(),ks) ||> Seq.fold(fun sb (k,c) -> sb.Append(Viz.ksMap.[k]).Append(c:int))
  sb.ToString()

let drawLegend margin (clusterTypes:Smap<Set<Knowledge*int>> ref) (m:Mat)  =
  let x = m.Width - margin - 100
  let disp = 60
  let lineLength = 70
  (!clusterTypes).Ordered  
  |> List.rev 
  |> List.iteri (fun i ks -> 
    let l = ksSetString ks
    let y = margin + (i * 12)
    Cv2.Line(!>m, Point(x+disp,y),Point(x+disp+lineLength,y),clusterColor i,10)
    Cv2.PutText(!>m, l, Point(x + 15, y + 5), HersheyFonts.HersheyPlain, 1., Scalar.White)
    )

let basePrimKs = (fun (k:Knowledge) -> k)
let inline fstPrimKs  (k:Knowledge,_) = k


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
