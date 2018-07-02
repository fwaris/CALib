﻿module Viz
open CA
open Schelling
open OpenCvSharp
open VizUtils

let brgColors = colors|> List.map (fun (r,g,b) -> Scalar(float b, float r, float g))
let ks = function 
    | Domain        -> 0 
    | Historical    -> 1 
    | Situational   -> 2 
    | Normative     -> 3 
    | Topgraphical  -> 4
    | _             -> failwith "shoud not happen"

let clrKnowledge (k:Knowledge) = brgColors.[ks k]

let visualizePopHex<'t> width (fc:'t->Scalar)  (network:Network<'t>) (pop:Population<'t>) (mat:Mat) =
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
        Cv2.Circle(!> mat,ctr,rad,clr,Cv2.FILLED)
    //pop |> Array.iter(fun p->draw p.Id brgColors.[2])
    pop |> Array.iter (fun p -> draw p.Id (fc p.KS))
    //pop |> Array.iter (fun p ->
    //    let i = p.Id
    //    draw i (fc p.KS)
    //    let ns = network pop p.Id
    //    for n in ns do draw n.Id (fc p.KS))

let drawLabel (m:Mat) y =
    let ks =
        [
            Domain      , "D"
            Historical  , "H"
            Situational , "S"
            Normative   , "N"
            Topgraphical, "T"
        ]
    ks |> List.iteri (fun i (k,l) ->
        let x = 52 * i + 30
        Cv2.Circle(!>m, Point(x,y),10,clrKnowledge k, Cv2.FILLED)
        Cv2.PutText(!>m, l, Point(x + 15, y + 5), HersheyFonts.HersheyPlain, 1., clrKnowledge k)
       )

let createVid ouput size maxGen (ca:CA<_>) clrF = 
    let ext = 20
    let margin = 5
    let pSize = Size(size, size + ext + 2 * margin)
    let enc = encoder ouput 30. (pSize.Width,pSize.Height)
    let drawFrame ca =
        let mParent = new Mat(pSize, MatType.CV_8UC3)
        drawLabel mParent (pSize.Height - ext - margin)
        let mChild = mParent.SubMat(Rect(0,0,size,size))
        visualizePopHex size clrF  ca.Network ca.Population mChild
        enc.Frame mParent
        mChild.Release()
        mParent.Release()
    let runCA maxBest (ca:CA<_>) =    
        for i in 1 .. 10 do drawFrame ca
        let loop stp = 
            let stp = CARunner.step false stp maxBest
            drawFrame stp.CA
            stp
        let step = {CA=ca; Best=[]; Count=0; Progress=[]}
        step 
        |> Seq.unfold (fun s -> let s = loop s in (s,s)  |> Some ) 
    let _ = runCA  2 ca |> Seq.take maxGen |> Seq.toArray
    enc.Release()

let visualizePopTest<'t> id width (network:Network<'t>) (pop:Population<'t>) =
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
