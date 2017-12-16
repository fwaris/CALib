module VizLandscape
//generate contour map of 2D fitness landscape
open System.Threading.Tasks

open System.IO
open System.Drawing
open System.Drawing.Drawing2D
open System.Windows.Forms
open VizUtils
open System.Drawing.Imaging
open System
open Microsoft.FSharp.NativeInterop

let private scaler (sMin,sMax) (vMin,vMax) (v:float) =
    if v < vMin then failwith "out of min range for scaling"
    if v > vMax then failwith "out of max range for scaling"
    (v - vMin) / (vMax - vMin) * (sMax - sMin) + sMin
    (*
    scaler (0.1, 0.9) (10., 500.) 223.
    scaler (0.1, 0.9) (10., 500.) 10.
    scaler (0.1, 0.9) (10., 500.) 500.
    scaler (0.1, 0.9) (-200., -100.) -110.
    *)

let createColor (r,g,b) = Color.FromArgb(255, min r 255, min g 255, min b 255)

let interpolate (c1:Color) (c2:Color) (m:float) =
    createColor(
        (1. - m) * float c1.R + float c2.R * m |> int,
        (1. - m) * float c1.G + float c2.G * m |> int,
        (1. - m) * float c1.B + float c2.B * m |> int
    )

let cI (v:float) mn mx (colorRange:Color[]) =
    if v < mn || v > mx then failwithf "value %0.3f should be between min (%0.3f) and max (%0.3f)" v mn mx
    let cindex = scaler (0., float (colorRange.Length - 1)) (mn,mx) v
    let c1 = colorRange.[floor cindex |> int]
    let c2 = colorRange.[ceil cindex |> int]
    let prop = cindex - (floor cindex)
    interpolate c1 c2 prop


let cb = [|Color.Lavender; Color.Lavender; Color.DarkBlue;  Color.DarkOrange; Color.Orange; Color.RosyBrown; Color.Red; Color.Maroon|]

#nowarn "9"
let genImage ((mX,mY,mZ),ftnss) =
    let imageHeight = 1024
    let imageWidth = 1024
    let pH1 = imageHeight - 1
    let pW1 = imageWidth - 1
    let clamp x = max (min x 1023) 0  
    let bmp = new Bitmap(imageWidth,imageHeight)
    let data = bmp.LockBits(
                new Rectangle(0, 0, bmp.Width, bmp.Height),
                ImageLockMode.ReadWrite,
                bmp.PixelFormat)
    let ptr = data.Scan0
    let ptr:nativeptr<byte> = NativePtr.ofNativeInt ptr
    let channelStride = data.Stride //4 // argb

    Parallel.For(0, imageHeight, fun h ->
        let y' = scaler (-1.,1.) (0.,float pH1) (float h)
        Parallel.For(0, imageWidth,  fun w ->
            let x' = scaler (-1.,1.) (0.,float pW1) (float w)
            let z' = ftnss [|x';y'|] 
            let c = cI (max z' 13.) 13. mZ cb
            let i = (pH1 - h) * channelStride + (w * 4)
            NativePtr.set ptr i c.B
            NativePtr.set ptr (i+1) c.G
            NativePtr.set ptr (i+2) c.R
            NativePtr.set ptr (i+3) 255uy
            ) |> ignore
            ) |> ignore
    bmp.UnlockBits(data)
    let mx = scaler (0., float pW1) (-1., 1.) mX |> int
    let my = scaler (0., float pH1) (-1., 1.) mY |> int
    for i in -20 .. 20 do
        bmp.SetPixel(mx + i |> clamp, pH1 - my |> clamp, Color.Black)
        bmp.SetPixel(mx |> clamp, pH1 - my + i |> clamp, Color.Black)        
    bmp

let genImage2 ((mX,mY,mZ),ftnss) =
    let pH = 1024
    let pH1 = pH - 1
    let pW = 1024
    let pW1 = pW - 1 
    let clamp x = max (min x 1024) 0  
    let bm = new Bitmap(pW,pH)
    for x in 0 .. 1 .. pH1 do
        let x' = scaler (-1.,1.) (0.,float pW1) (float x)
        for y in 0 .. 1 .. pW1 do
            let y' = scaler (-1.,1.) (0.,float pH1) (float y)
            let h = ftnss [|x'; y'|]
            let c = cI (max h 13.) 13. mZ cb
            //printfn "%d,%d - %f,%f - %f - %A" x y x' y' h c
            bm.SetPixel(x, pH1 - y, c)
    let mx = scaler (0., float pW1) (-1., 1.) mX |> int
    let my = scaler (0., float pH1) (-1., 1.) mY |> int
    for i in -20 .. 20 do
        bm.SetPixel(mx + i |> clamp, pH1 - my |> clamp, Color.NavajoWhite)
        bm.SetPixel(mx |> clamp, pH1 - my + i |> clamp, Color.NavajoWhite)        
    bm


let showLandscape (bmp:Bitmap) = 
    let form = new Form()
    form.Width  <- 400
    form.Height <- 400
    form.Visible <- true 
    form.Text <- "Image"
    let p = new PictureBox(
                    Image=bmp,
                    Dock = DockStyle.Fill,
                    SizeMode=PictureBoxSizeMode.StretchImage)
    form.Controls.Add(p)
    form.Show()

open DF1
open OpenCvSharp

let gen (cone,ftnss) = genImage ((cone.L.[0],cone.L.[1],cone.H),ftnss)

let createVid ouput n world = 
    let enc = encoder ouput 1. (1024,1024)
    let gen = landscape >> gen
    let drawFrame m =
        let matA = OpenCvSharp.Extensions.BitmapConverter.ToMat(m)
        let mat = new Mat(matA.Size(), MatType.CV_8UC3)
        Cv2.CvtColor(!>matA,!>mat, ColorConversionCodes.BGRA2BGR)
        enc.Frame mat
        mat.Release()
        matA.Release()
    let w1 = gen world
    for i in 1 .. 2 do drawFrame w1
    for i in 1 .. n do
        let w = updateWorld world
        let m = gen w
        drawFrame m
    enc.Release()
