module VizUtils
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

open System.Drawing

//let scaler (sMin,sMax) (vMin,vMax) (v:float) =
//    if v < vMin then failwith "out of min range for scaling"
//    if v > vMax then failwith "out of max range for scaling"
//    (v - vMin) / (vMax - vMin) * (sMax - sMin) + sMin

let createColor (r,g,b) = Color.FromArgb(255, min r 255, min g 255, min b 255)

let interpolate (c1:Color) (c2:Color) (m:float) =
    createColor(
        (1. - m) * float c1.R + float c2.R * m |> int,
        (1. - m) * float c1.G + float c2.G * m |> int,
        (1. - m) * float c1.B + float c2.B * m |> int
    )

let cI (v:float) mn mx (colorRange:Color[]) =
    //if v < mn || v > mx then failwithf "value %0.3f should be between min (%0.3f) and max (%0.3f)" v mn mx
    let cindex = CAUtils.scaler (0., float (colorRange.Length - 1)) (mn,mx) v
    let c1 = colorRange.[floor cindex |> int]
    let c2 = colorRange.[ceil cindex |> int]
    let prop = cindex - (floor cindex)
    interpolate c1 c2 prop

//usage:
// cI 0.3 0. 1. [|Color.Blue; Color.Yellow; Color.Red|]