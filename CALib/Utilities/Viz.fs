module Viz
open CA
open Schelling
open OpenCvSharp

let uiCtx = System.Threading.SynchronizationContext.Current

let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

let winAsync t i =
    async{
        do! Async.SwitchToContext uiCtx
        new Window((t:string), WindowMode.AutoSize,i) |> ignore 
        } 

let win t i = winAsync t i |> Async.Start

type IEncoder =
    abstract member Frame : Mat -> unit
    abstract member Release : unit -> unit

let private captureTo v_out frameRate sz =
    let clipOut = new VideoWriter()
    clipOut.Open(v_out,FourCC.DIVX,frameRate,sz)
    if not(clipOut.IsOpened()) then failwith "file not opened"
    clipOut

let encoder file frameRate (w:int,h:int) =
    let sz = Size(w,h)
    let clipOut = captureTo file frameRate sz
    {new IEncoder with 
        member x.Frame mat = clipOut.Write mat
        member x.Release() = clipOut.Release()
        }

let colors = 
        [
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
        ]

let brgColors = colors|> List.map (fun (r,g,b) -> Scalar(float b, float r, float g))
let ks = function Domain -> 0 | Historical -> 1 | Situational -> 2 | Normative -> 3 | Topgraphical -> 4| _ -> failwith "shoud not happen"

let clrKnowledge (k:Knowledge) = brgColors.[ks k]

let visualizePopHex<'t> (fc:'t->Scalar) width (network:Network<'t>) (pop:Population<'t>) =
    let drawn = Array.create pop.Length 0
    let sz = Size(int width, int width)
    let mat = new Mat(sz,MatType.CV_8UC3)
    let rowCount = sqrt (float pop.Length)
    let halfRc = rowCount / 2.0 |> ceil
    let rowLen = int rowCount
    let w = float sz.Width
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
        Cv2.Circle(!> mat,ctr,rad,clr,Cv2.FILLED)
    //pop |> Array.iter(fun p->draw p.Id brgColors.[2])
    pop |> Array.iter (fun p -> draw p.Id (fc p.KS))
    //pop |> Array.iter (fun p ->
    //    let i = p.Id
    //    draw i (fc p.KS)
    //    let ns = network pop p.Id
    //    for n in ns do draw n.Id (fc p.KS))
    mat

let visualizePopTest<'t> id width (network:Network<'t>) (pop:Population<'t>) =
    let drawn = Array.create pop.Length 0
    let sz = Size(int width, int width)
    let mat = new Mat(sz,MatType.CV_8UC3)
    let rowCount = sqrt (float pop.Length)
    let halfRc = rowCount / 2.0 |> ceil
    let rowLen = int rowCount
    let w = float sz.Width
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
        Cv2.Circle(!> mat,ctr,rad,clr,Cv2.FILLED)
    //pop |> Array.iter(fun p->draw p.Id brgColors.[2])
    pop |> Array.iter (fun p ->
        let i = p.Id
        if i = id then
            draw i (Scalar(100.,200.,10.))
            let ns = network pop p.Id
            for n in ns do draw n.Id (Scalar(10.,200.,100.)))
    mat

let createVid ouput size maxGen (ca:CA<_>) clrF = 
    let enc = encoder ouput 30. (size,size)
    let runCA maxBest (ca:CA<_>) =
        let m = visualizePopHex clrF size ca.Network ca.Population
        for i in 1 .. 10 do enc.Frame m
        m.Release()
        let loop stp = 
            let stp = CARunner.step stp maxBest
            let m = visualizePopHex clrF size stp.CA.Network stp.CA.Population
            enc.Frame m
            m.Release()
            stp
        let step = {CA=ca; Best=[]; Count=0; Progress=[]}
        step 
        |> Seq.unfold (fun s -> let s = loop s in (s,s)  |> Some ) 

    let _ = runCA  2 ca |> Seq.take maxGen |> Seq.toArray
    enc.Release()

