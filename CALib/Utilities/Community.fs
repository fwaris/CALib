module Community
open TracingGame
open CAUtils
open VizUtils
open System
open OpenCvSharp
open KDIPDGame

let ipdClr ((k,_):KDIPDGame.IpdKS) = Viz.brgColors.[Viz.ks k.KS]

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
  let support = 2 
  let itemCount = 2
  let itemSets = FPGrowth.mineTree support tree 
  let largeItemSets = itemSets |> Seq.filter (fun (a,_) -> a.Count >= itemCount) |> Seq.map fst
  let clusters = largeItemSets |> Seq.map(fun is -> 
    let isCount = Set.count is
    let matchTxns = 
      baskets 
      |> Array.mapi (fun i s -> let s' = Set.intersect s is in if Set.count s' = isCount then Some i else None) 
      |> Array.choose yourself
    is,matchTxns)
  clusters
  //printfn "%A" clusters
  //()

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
   |> Array.map (fun (b,g,r) -> Scalar(float b,float g,float r))


let drawFrame (pSize:Size) size ext margin network pop =
  let mParent = new Mat(pSize, MatType.CV_8UC3)
  Viz.drawLabel mParent (pSize.Height - ext - margin)
  let mChild = mParent.SubMat(Rect(0,0,size,size))
  Viz.visualizePopHex size ipdClr network pop mChild
  mChild,mParent
  //mChild.Release()
  //mParent.Release()

let drawClusters width (network:CA.Network<_>) (pop:CA.Population<_>) (mat:Mat) (clusters:seq<Set<CA.Id>*int[]>)=
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
    let draw i clr = 
        let r = i / rowLen 
        let c = i % rowLen
        let shiftY = if c % 2 = 0 then halfIncr else -halfIncr
        let y = margin + (float r * incr + shiftY)
        let x = margin + (float c * incr)
        let ctr = Point(int x, int y)
        Cv2.Circle(!> mat,ctr,rad+1,clr,2)
    //pop |> Array.iter(fun p->draw p.Id brgColors.[2])
    clusters |> Seq.iteri(fun i (s,ids) ->
      let community = Seq.append s ids |> Seq.toArray
      let clr  = communityColors.[i % communityColors.Length]
      community |> Array.iter(fun i -> draw i clr)
    )

let visCommunity file (obs:IObservable<SG<IpdKS>>) =
  let ext = 20
  let margin = 5
  let size = 512
  let pSize = Size(size, size + ext + 2 * margin)
  let enc = encoder file 30. (pSize.Width,pSize.Height)

  let disp = 
    obs.Subscribe(fun {Pop=pop;Net=network;Vmin=vmn;Links=links} ->
      for i in 1 .. 10 do 
        let mC,mP=drawFrame pSize size ext margin network pop
        let clusters = cluster pop.Length vmn links
        drawClusters size network pop mC clusters
        enc.Frame mP
        mC.Release()
        mP.Release()
        printfn "."
    )

  disp,enc
