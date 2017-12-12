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


