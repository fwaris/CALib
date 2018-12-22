
#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load "..\Utilities\VizUtils.fs"
#load "..\Utilities\VizNetwork.fs"
#load "..\DF1.fs"
#load "TestViz.fsx"
open TestEnv
open CA
open CAUtils
open DF1
open TestEnv
open OpenCvSharp
open TestViz
open System.Drawing
open System.IO
open System
open VizUtils
open Viz
open OpenCvSharp
open OpenCvSharp.ML

let remE = StringSplitOptions.RemoveEmptyEntries

let joboutFolder =  @"D:\repodata\calib\jobout"
let outfolder = @"D:\repodata\calib\dsst_soc_all_gen"
if Directory.Exists outfolder |> not then Directory.CreateDirectory outfolder |> ignore
let (@@) a b = Path.Combine(a,b)

let jobOuts = Directory.EnumerateDirectories joboutFolder |> Seq.toArray
let kd_a = jobOuts |> Array.map (fun x->let i = x.LastIndexOf('_') in x.Substring(0,i)) |> Array.distinct
let sample1s = kd_a |> Array.map(fun x->x + "_1")

type SocR = {KD:string; A:string; Landscape:int; Gen:int; Sample:int; Segs:float[]; Dffns:float[]; KS:int[]}

let toSocrs rows = rows |> Array.map (fun (r:string[]) ->
   {
    KD          = r.[1]
    Landscape   = int r.[2]
    A           = r.[3]
    Gen         = int r.[4]
    Sample      = int r.[0]
    Segs        = r.[9].Split([|'|'|],remE) |> Array.map float
    Dffns       = r.[10].Split([|'|'|],remE) |> Array.map float
    KS          = r.[11].Split([|'|'|],remE) |> Array.map int
   })

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
        Cv2.PutText(!>mat, kLetter, txtPt, HersheyFonts.HersheyPlain, 1., Scalar.DarkBlue)
    //pop |> Array.iter(fun p->draw p.Id brgColors.[2])
    pop |> Array.iteri (fun i p -> draw i (clrF p))
    //pop |> Array.iter (fun p ->
    //    let i = p.Id
    //    draw i (fc p.KS)
    //    let ns = network pop p.Id
    //    for n in ns do draw n.Id (fc p.KS))

let drawLabelGen (idx:int) (m:Mat) y gen lndscp =
    ksMap |> Seq.iteri (fun i kv ->
        let k = kv.Key
        let l = kv.Value
        let x = 52 * i + 30
        Cv2.Circle(!>m, Point(x,y),10,clrKnowledge k, Cv2.FILLED)
        Cv2.PutText(!>m, l, Point(x + 15, y + 5), HersheyFonts.HersheyPlain, 1., clrKnowledge k)
       )
    let x' = (50 * ksMap.Count) + 30
    let p = Point(x'+ 15, y + 5)
    Cv2.PutText(!>m, lndscp.ToString(),p, HersheyFonts.HersheyTriplex, 1., Scalar.Wheat)
    Cv2.PutText(!>m, gen.ToString(),Point(p.X + 50, p.Y), HersheyFonts.HersheyTriplex, 1., Scalar.Azure)
    if gen=1 then //1 generation after maxgen - draw rect to highlight environment change
        Cv2.Circle(!>m, Point(p.X + 450,p.Y),10,Scalar.Maroon, Cv2.FILLED)
    
let createVidHeat ouput size popSeq ksSeq clrF (gens:int[]) (lndscps:int[]) = 
    let ext = 20
    let margin = 5
    let pSize = Size(size, size + ext + 2 * margin)
    let enc = encoder ouput 30. (pSize.Width,pSize.Height)
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

let dfsnClr v =
    let clr = VizUtils.cI v 0. 2.0 [|Color.Blue; Color.Yellow; Color.Red|]
    Scalar.FromRgb(int clr.R, int clr.G, int clr.B) 

let segClr v = 
    let clr = VizUtils.cI v 0. 2. [|Color.Blue; Color.Yellow; Color.Red|]
    Scalar.FromRgb(int clr.R, int clr.G, int clr.B) 


let genVidsByKdA socrs =
    let kdA = socrs |> Array.groupBy (fun r->r.KD,r.A)

    kdA |> Array.iter (fun ((kd,a),srs) -> 
        let segs = srs |> Array.map(fun j->j.Segs)
        let dffns = srs |> Array.map(fun j->j.Dffns)
        let kss   = srs |> Array.map(fun j->j.KS)
        let gens  = srs |> Array.map(fun j->j.Gen)
        let lndscps = srs |> Array.map(fun j->j.Landscape)
        let fseg = outfolder @@ (sprintf "%s_%s_segs.mov" kd a)
        let fdffsn = outfolder @@ (sprintf "%s_%s_dffns.mp4" kd a)
        createVidHeat fseg 1024 segs kss segClr gens lndscps 
        createVidHeat fdffsn 1024 dffns kss dfsnClr gens lndscps )
  
let gen1Sample()=
    let statFiles = sample1s |> Array.map(fun p-> Path.Combine(p,"Stats.txt"))
    let rows = 
        seq {for f in statFiles do yield! (File.ReadLines f |> Seq.skip 1) } 
        |> Seq.map(fun x->x.Split([|'\t'|])) |> Seq.toArray
    let socrs = toSocrs rows |> Array.filter(fun r->r.Sample=1)
    genVidsByKdA socrs


let gen2Sample() =
    let statFile = @"D:\repodata\calib\video_loc\Stats.txt"
    let rows = File.ReadLines statFile |> Seq.skip 1 |> Seq.map(fun x->x.Split([|'\t'|])) |> Seq.toArray
    let socrs = toSocrs rows |> Array.filter(fun r-> r.Sample=1)// && r.Landscape=2)
    let maxGen = socrs |> Array.map(fun r->r.Gen) |> Array.max
    genVidsByKdA socrs