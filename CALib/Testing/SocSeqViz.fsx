(*
video generation from CA run over a sequence of dynamic landscapes (experimental)
*)
#load "TraceDynamic.fsx"
open CA
open CAUtils
open OpenCvSharp
open Runs.Environment
open Config.Types
open Runs.Types
open System.Drawing
open System
open System.IO
open OpenCvSharp

//utility operator for F# implicit conversions 
let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

let rsc = 
    {
      SaveFolder    = @"d:\calib\dsst_stats"
      EnvChngSensitivity = [0]
      Restartable   = true
      KDs            = [WTD; IPD; SHS; STK]
      PopulationSize = 72
      NumCones      = 1000
      RunToMax      = false
      CalcSocMetrics = false
      MaxGen        = 250 //2500
      NumLandscapes = 50
      Samples       = 1
      DistTh        = 0.001
      AValues       = [3.1]
      ChangeHeight  = false
      ChangeRadius  = false
      ChangeLoc     = true
    }

let ws = createEnv rsc 3.1
let f : Fitness = ref ws.F

let basePop = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    CAUtils.createPop (baseKsInit bsp) parmDefs rsc.PopulationSize true

open FSharp.Collections.ParallelSeq

let remE = StringSplitOptions.RemoveEmptyEntries

let joboutFolder =  @"D:\calib\jobout"
let outfolder = @"D:\calib\dsst_soc_all_gen"
if Directory.Exists outfolder |> not then Directory.CreateDirectory outfolder |> ignore
let (@@) a b = Path.Combine(a,b)

let jobOuts = Directory.EnumerateDirectories joboutFolder |> Seq.toArray
let kd_a = jobOuts |> Array.map (fun x->let i = x.LastIndexOf('_') in x.Substring(0,i)) |> Array.distinct
let sample1s = kd_a |> Array.map(fun x->x + "_1")

type SocR = {KD:string; A:string; Landscape:int; 
             EnvSnstvty:int;
             Gen:int; Sample:int; Segs:float[]; Dffns:float[]; KS:int[]}

let toSocrs rows = rows |> Array.map (fun (r:string[]) ->
   {
    Sample      = int r.[0]
    KD          = r.[1]
    EnvSnstvty  = int r.[2]
    Landscape   = int r.[3]
    A           = r.[4]
    Gen         = int r.[5]
    Segs        = r.[11].Split([|'|'|],remE) |> Array.map float
    Dffns       = r.[12].Split([|'|'|],remE) |> Array.map float
    KS          = r.[13].Split([|'|'|],remE) |> Array.map int
   })

let dfsnClr v =
    let clr = VizUtils.cI v 0. 2.0 [|Color.Blue; Color.Yellow; Color.Red|]
    Scalar.FromRgb(int clr.R, int clr.G, int clr.B) 

let segClr v = 
    let clr = VizUtils.cI v 0. 2. [|Color.Blue; Color.Yellow; Color.Red|]
    Scalar.FromRgb(int clr.R, int clr.G, int clr.B) 

//let maxSeg = socrs |> Array.map (fun x-> Array.max x.Segs) |> Array.max
//let maxDffsn = socrs |> Array.map (fun x-> Array.max x.Dffns) |> Array.max

let neighbrs (pop:float[]) id =
    let rowCount = sqrt (float pop.Length)
    let rowLen = int rowCount
    let r = id / rowLen
    let c = id % rowLen
    let evnCol = if c % 2 = 0 then 1 else -1
    let idxs = 
        [|
            r * rowLen + c-1
            r * rowLen + c+1
            (r-1) * rowLen + c
            (r+1) * rowLen + c
            (r+evnCol) * rowLen + (c-1)
            (r+evnCol) * rowLen + (c+1)
        |]
    idxs |> Array.map (fun i-> pop.[if i < 0 then pop.Length+i else i % pop.Length])

let ksTags =
    [Situational; Historical; Normative; Topgraphical; Domain]
    |> List.map (fun x->Social.ksNum x,x)// x |> string |> Seq.head |> string )
    |> Map.ofList

let visualizePopHeat width (pop:float[]) (ks:int[]) (clrF:float->Scalar) (mat:Mat) =
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
    let rad = incr/2.0 |> ceil |> int 
    let draw i clr = 
        let r = i / rowLen 
        let c = i % rowLen
        let shiftY = if c % 2 = 0 then halfIncr else -halfIncr
        let y = margin + (float r * incr + shiftY)
        let x = margin + (float c * incr)
        let ctr = Point(int x, int y)
        let k = ksTags.[ks.[i]]
        let kLetter = Viz.ksMap.[k]
        let c2 = Viz.clrKnowledge k
        Cv2.Circle(!> mat,ctr,rad,clr,Cv2.FILLED)
        Cv2.Circle(!> mat,ctr,rad/2,c2,Cv2.FILLED)
        Cv2.Circle(!> mat,ctr,rad/2+1,Scalar.Wheat,1)
        let txtPt = Point(ctr.X-4,ctr.Y+5)
        //Cv2.PutText(!>mat, (string i), txtPt, HersheyFonts.HersheyPlain, 1., Scalar.DarkBlue)
        Cv2.PutText(!>mat, kLetter, txtPt, HersheyFonts.HersheyPlain, 1., Scalar.DarkBlue)
    //pop |> Array.iter(fun p->draw p.Id brgColors.[2])
    pop |> Array.iteri (fun i p -> draw i (clrF p))
    //pop |> Array.iter (fun p ->
    //    let i = p.Id
    //    draw i (fc p.KS)
    //    let ns = network pop p.Id
    //    for n in ns do draw n.Id (fc p.KS))

let visualizePop width (pop:float[]) (ks:int[]) (clrF:float->Scalar) (mat:Mat) =
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
    let rad = incr/2.0 |> ceil |> int 
    let draw i clr = 
        let r = i / rowLen 
        let c = i % rowLen
        let shiftY = if c % 2 = 0 then halfIncr else -halfIncr
        let y = margin + (float r * incr + shiftY)
        let x = margin + (float c * incr)
        let ctr = Point(int x, int y)
        let k = ksTags.[ks.[i]]
        let kLetter = Viz.ksMap.[k]
        let c2 = Viz.clrKnowledge k
        //Cv2.Circle(!> mat,ctr,rad,clr,Cv2.FILLED)
        Cv2.Circle(!> mat,ctr,rad,c2,Cv2.FILLED)
        Cv2.Circle(!> mat,ctr,rad/2+1,Scalar.Wheat,1)
        let txtPt = Point(ctr.X-11,ctr.Y+8)
        //Cv2.PutText(!>mat, (string i), txtPt, HersheyFonts.HersheyPlain, 1., Scalar.DarkBlue)
        Cv2.PutText(!>mat, kLetter, txtPt, HersheyFonts.HersheyComplexSmall, 1.5, Scalar.DarkBlue)
    //pop |> Array.iter(fun p->draw p.Id brgColors.[2])
    pop |> Array.iteri (fun i p -> draw i (clrF p))
    //pop |> Array.iter (fun p ->
    //    let i = p.Id
    //    draw i (fc p.KS)
    //    let ns = network pop p.Id
    //    for n in ns do draw n.Id (fc p.KS))

let drawLabelGen (idx:int) (m:Mat) y seg lndscp =
    Viz.ksMap |> Seq.iteri (fun i kv ->
        let k = kv.Key
        let l = kv.Value
        let x = 52 * i + 30
        Cv2.Circle(!>m, Point(x,y),10,Viz.clrKnowledge k, Cv2.FILLED)
        Cv2.PutText(!>m, l, Point(x + 15, y + 5), HersheyFonts.HersheyPlain, 1., Viz.clrKnowledge k)
       )
    let x' = (50 * Viz.ksMap.Count) + 30
    let p = Point(x'+ 15, y + 5)
    Cv2.PutText(!>m, lndscp.ToString(),p, HersheyFonts.HersheyTriplex, 1., Scalar.Wheat)
    Cv2.PutText(!>m, seg.ToString(),Point(p.X + 50, p.Y), HersheyFonts.HersheyTriplex, 1., Scalar.Azure)
    if seg=1 then //1 generation after maxgen - draw rect to highlight environment change
        Cv2.Circle(!>m, Point(p.X + 450,p.Y),10,Scalar.Maroon, Cv2.FILLED)
    
let createVidHeat ouput size popSeq ksSeq clrF (gens:int[]) (lndscps:int[]) = 
    let ext = 20
    let margin = 5
    let pSize = Size(size, size + ext + 2 * margin)
    let enc = VizUtils.encoder ouput 30. (pSize.Width,pSize.Height)
    let drawFrame i (pop, ks) =
        let mParent = new Mat(pSize, MatType.CV_8UC3,  Scalar(0., 0., 0.))
        let mChild = mParent.SubMat(Rect(0,0,size,size))
        visualizePopHeat size pop ks clrF mChild
        drawLabelGen i mParent (pSize.Height - ext - margin) (gens.[i]) (lndscps.[i])
        enc.Frame mParent
        mChild.Release()
        mParent.Release()
    Seq.zip popSeq ksSeq  |> Seq.iteri(fun i m ->
        for _ in 1 .. 1 do 
            drawFrame i m)

    enc.Release()



let begAft maxGen (xs:SocR array) =
    xs |> Array.filter (fun x->x.Gen % maxGen = 0 || x.Gen % maxGen = 1)


let genVidsByKdA socrs =
    let maxGen = socrs |> Array.map (fun g -> g.Gen) |> Array.max
    let kdA = socrs |> Array.groupBy (fun r->r.KD,r.A)

    kdA |> Array.iter (fun ((kd,a),srs) -> 
        let srs = begAft maxGen srs
        let segs = srs |> Array.map(fun j->j.Segs)
        //let dffns = srs |> Array.map(fun j->j.Dffns)
        let kss   = srs |> Array.map(fun j->j.KS)
        let gens  = srs |> Array.map(fun j->j.Gen)
        let lndscps = srs |> Array.map(fun j->j.Landscape)
        let fseg = outfolder @@ (sprintf "%s_%s_segs.mov" kd a)
        //let fdffsn = outfolder @@ (sprintf "%s_%s_dffns.mp4" kd a)
        createVidHeat fseg 1024 segs kss segClr gens lndscps 
//        createVidHeat fdffsn 1024 dffns kss dfsnClr gens lndscps 
        )
  
let gen1Sample()=
    let statFiles = sample1s |> Array.map(fun p-> Path.Combine(p,"Stats.txt"))
    let rows = 
        seq {for f in statFiles do yield! (File.ReadLines f |> Seq.skip 1) } 
        |> Seq.map(fun x->x.Split([|'\t'|])) |> Seq.toArray
    let socrs = toSocrs rows |> Array.filter(fun r->r.Sample=1)
    genVidsByKdA socrs


let gen2Sample() =
    let statFile = @"D:\calib\jobout\Stats.txt"
    let rows = File.ReadLines statFile |> Seq.skip 1 |> PSeq.map(fun x->x.Split([|'\t'|])) |> PSeq.toArray
    let socrs_ = toSocrs rows |> Array.filter(fun r-> r.Sample=1)// && r.Landscape=2)
    let socrs = socrs_ |> PSeq.sortBy (fun x->x.KD,x.A,x.Sample,x.Landscape,x.Gen ) |> PSeq.toArray
    genVidsByKdA socrs


let genVids() =
    let folder = @"D:\calib\jobout_sample"
    let files = folder |> Directory.GetFiles
    let rows = 
        seq {for f in files do yield! (File.ReadLines f |> Seq.skip 1) } 
        |> Seq.map(fun x->x.Split([|'\t'|])) |> Seq.toArray
    let socrs = toSocrs rows |> Array.filter(fun x->x.EnvSnstvty = 0) |> Array.sortBy (fun x->x.KD,x.A,x.Landscape,x.Gen)
    genVidsByKdA socrs


type SegSample = 
    {
        Sample      : int
        Kd          : string
        EnvSnstvty  : int
        A           : float
        Pick        : string
        Seg         : float
        IndvKS      : int[]
        IndvSegs    : float[]
    }


let toSampleSeg (xs:string[]) = 
    
    {
        Sample      = int xs.[0]
        Kd          = xs.[1]
        EnvSnstvty  = int xs.[2]
        A           = float xs.[3]
        Pick        = xs.[4]
        Seg         = float xs.[5]
        IndvKS      = xs.[6].Split('|') |> Array.map int
        IndvSegs    = xs.[7].Split('!') |> Array.map float
    }

let hexagonNetworkViz (pop:int[]) id =
    let rowCount = sqrt (float pop.Length)
    let rowLen = int rowCount
    let r = id / rowLen
    let c = id % rowLen
    let evnCol = if c % 2 = 0 then 1 else -1
    let idxs = 
        [|
            r * rowLen + c-1
            r * rowLen + c+1
            (r-1) * rowLen + c
            (r+1) * rowLen + c
            (r+evnCol) * rowLen + (c-1)
            (r+evnCol) * rowLen + (c+1)
        |]
    let idxAdj = idxs |> Array.map (fun i-> if i < 0 then pop.Length+i else i % pop.Length)
    idxAdj

let rec nhbrTypes (pop:int[]) radius ks  =
    seq {
        yield! ks
        if radius > 0 then
            let nbrs = ks |> Array.collect (fun (id,_) -> 
                hexagonNetworkViz pop id |> Array.map (fun i -> i,pop.[i]))
            yield! nhbrTypes pop (radius-1) nbrs
    }

let SEG_RAD = 1

let segregationAt radius pop indv =
    let segs = [1;2;3;4;5] // five ks or segments
    let prop = 1.0 / float segs.Length
    let nbrs = nhbrTypes pop radius [|indv,pop.[indv]|] |> Seq.toArray
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

let drawLabelSeg (idx:int) (m:Mat) y seg  =
    Viz.ksMap |> Seq.iteri (fun i kv ->
        let k = kv.Key
        let l = kv.Value
        let x = 52 * i + 30
        Cv2.Circle(!>m, Point(x,y),10,Viz.clrKnowledge k, Cv2.FILLED)
        Cv2.PutText(!>m, l, Point(x + 15, y + 5), HersheyFonts.HersheyPlain, 1., Viz.clrKnowledge k)
       )
    let x' = (50 * Viz.ksMap.Count) + 30
    let p = Point(x'+ 15, y + 5)
    Cv2.PutText(!>m, "Seg=",p, HersheyFonts.HersheyTriplex, 1., Scalar.Wheat)
    Cv2.PutText(!>m, sprintf "%0.2f" seg,Point(p.X + 100, p.Y), HersheyFonts.HersheyTriplex, 1., Scalar.Azure)
    let sY = y - 150
    Cv2.PutText(!>m, "Seg. Scale",Point(5, sY+40), HersheyFonts.HersheyDuplex, 1., Scalar.Azure)
    [0.0; 0.5; 1.0; 1.5; 2.0]
    |> List.iteri (fun i cx -> 
        let x = 75 * i + 200
        Cv2.Rectangle(m, Point(x,sY), Point(x+70,sY+70), segClr cx, Cv2.FILLED)
        let clr = if i = 0 then Scalar.LightGray else Scalar.Black
        Cv2.PutText(!>m,string cx, Point(x+5,sY+40), HersheyFonts.HersheyDuplex, 1.2, clr )
        )


let genFramesSegSample() =
    let inputFile =  @"D:\calib\dsst_stats\hi_low_seg_examples.txt"
    //let folder = Path.GetDirectoryName(inputFile) + @"\smpls_seg"
    let folder = Path.GetDirectoryName(inputFile) + @"\smpls_no_seg"
    if Directory.Exists folder |> not then Directory.CreateDirectory folder |> ignore
    let rs = 
        File.ReadLines inputFile 
        |> Seq.skip 1
        |> Seq.map (fun x-> x.Split('\t') |> toSampleSeg) |> Seq.toArray

    let segs = rs |> Array.map (fun x-> 
        let segs =   x.IndvKS |> Array.mapi (fun i _ -> segregationAt SEG_RAD x.IndvKS i)
        {| x with CalcSegs=segs; CalcAvgSeg=Array.average segs|})

    let size = 1024
    let ext = 20
    let margin = 5
    let pSize = Size(size, size + ext + 2 * margin)
    let saveImage file i (pop, ks) seg =
        let mParent = new Mat(pSize, MatType.CV_8UC3,  Scalar(0., 0., 0.))
        let mChild = mParent.SubMat(Rect(0,0,size,size))
        //visualizePopHeat size pop ks segClr mChild
        visualizePop size pop ks segClr mChild
        drawLabelSeg i mParent (pSize.Height - ext - margin) seg // (gens.[i]) (lndscps.[i])
        mParent.SaveImage(file) |> ignore
        mChild.Release()
        mParent.Release()

    segs |> Array.iter (fun s->
        let fn = sprintf "%s_%0.1f_%s.png" s.Kd s.A s.Pick
        let file = Path.Combine(folder,fn)
        saveImage file  0  (s.CalcSegs, s.IndvKS) s.CalcAvgSeg )
        

