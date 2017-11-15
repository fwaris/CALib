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
        250,45,210
        126,239,213
        50,228,181
        100,218,185
        238,232,170
        76,230,140
        189,183,107
        255,215,0
        ]

let brgColors = colors|> List.map (fun (r,g,b) -> Scalar(float b, float r, float g))
let ks = function Domain -> 0 | Historical -> 1 | Situational -> 2 | Normative -> 3| _ -> failwith "shoud not happen"

let visualizePop4 width (network:Network<SchKs>) (pop:Population<SchKs>) =
    let sz = Size(int width, int width)
    let mat = new Mat(sz,MatType.CV_8UC3)
    let pL = pop.Length
    let rc = sqrt (float pL) |> ceil 
    let rowLen = int rc
    let w = float sz.Width
    let margin = (w / rc) |> ceil
    //printfn "margin = %f" margin
    let cl = w - 2. * margin
    let incr = cl / rc 
    let rad = incr/4.0 |> ceil |> int
    pop |> Array.iteri (fun i p ->
        let r = i / rowLen 
        let c = i % rowLen
        let y = margin + (float r * incr)
        let x = margin + (float c * incr)
        let ctr = Point(int x, int y)
        //printfn "x,y = %d %d" ctr.X ctr.Y
        Cv2.Circle(!> mat,ctr,rad,Scalar(125.,123.,250.),Cv2.FILLED)
        )
    mat
  
let visualizePop6 width (network:Network<Knowledge>) (pop:Population<Knowledge>) =
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
        draw i brgColors.[ks p.KS]
        let ns = network pop p.Id
        for n in ns do draw n.Id brgColors.[ks n.KS])
    mat

