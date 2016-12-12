#load "..\DF1.fs"
open DF1
open System.IO
open System.Drawing
open System.Drawing.Drawing2D

let landscapes = [
    "1.01", @"test_cone1.01.csv"
    "2.0", @"test_cone2.0.csv"
    "3.35", @"test_cone3.35.csv"
    "3.5", @"test_cone3.5.csv"
    "3.99", @"test_cone3.99.csv"
    ]

let df1s = [for l in landscapes -> createDf1 (__SOURCE_DIRECTORY__ + "/" + snd l)]

let scaler (sMin,sMax) (vMin,vMax) (v:float) =
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

let genImage (n:Cone,f) =
    let pH = 1024
    let pH1 = pH - 1
    let pW = 1024
    let pW1 = pW - 1 
    let bm = new Bitmap(pW,pH)
    for x in 0 .. 1 .. pH1 do
        let x' = scaler (-1.,1.) (0.,float pW1) (float x)
        for y in 0 .. 1 .. pW1 do
            let y' = scaler (-1.,1.) (0.,float pH1) (float y)
            let h = f x' y'
            let c = cI (max h 13.) 13. n.H cb
            //printfn "%d,%d - %f,%f - %f - %A" x y x' y' h c
            bm.SetPixel(x, pH1 - y, c)
    let mx = scaler (0., float pW1) (-1., 1.)  n.X |> int
    let my = scaler (0., float pH1) (-1., 1.) n.Y |> int
    for i in -10 .. 10 do
        bm.SetPixel(mx + i,pH1 - my, Color.NavajoWhite)
        bm.SetPixel(mx, pH1 - my + i, Color.NavajoWhite)        
    bm

let run() =
    df1s 
//    |> Seq.take 1
    |> Seq.iteri (fun i l ->
        let b = genImage l
        let fn = snd landscapes.[i] |> Path.GetFileNameWithoutExtension
        let p1 = __SOURCE_DIRECTORY__ + "/" + fn + ".png"
        b.Save(p1,Imaging.ImageFormat.Png))


//cI 0.3 0. 1. [|Color.Blue; Color.Yellow; Color.Red|]