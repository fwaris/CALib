#r @"..\..\packages\FSharp.Collections.ParallelSeq\lib\net40\FSharp.Collections.ParallelSeq.dll"
#load "..\Testing\SetupVideo.fsx"
#load "..\Probability.fs"
#load "..\Utilities\VizUtils.fs"
#load "..\DF1.fs"
#load "..\Utilities\VizLandscape.fs"
open DF1
open System.IO
open System.Drawing
open System.Drawing.Drawing2D
open VizLandscape

let landscapes = [
    "1.01", @"test_cone1.01.csv"
    "2.0", @"test_cone2.0.csv"
    "3.35", @"test_cone3.35.csv"
    "3.5", @"test_cone3.5.csv"
    "3.99", @"test_cone3.99.csv"
    ]

let df1s = [for l in landscapes -> createDf1 (__SOURCE_DIRECTORY__ + "/" + snd l)]

let test1() =
    let (c,f) = df1s.[3]
    gen(c,f) |> showLandscape

let test2()=
    let w = createWorld 500 2 (5.,15.) (20., 10.) None None (Some 3.99)
    let (c,f) = landscape w
    gen (c,f) |> showLandscape
    updateWorld w |> landscape |> gen |> showLandscape

let test3() =
    let w = createWorld 500 2 (5.,15.) (20., 10.) None None (Some 3.99)
    createVid @"D:\repodata\calib\landscape1.mp4" 100 w

let test4() =
    let w = createWorld 500 2 (5.,15.) (20., 10.) None None (Some 3.99) |> ref
    w := updateWorld !w
    landscape !w |> gen |> showLandscape

let run() =
    df1s 
//    |> Seq.take 1
    |> Seq.iteri (fun i (cone,ftnss) ->
        let b = gen(cone,ftnss)
        let fn = snd landscapes.[i] |> Path.GetFileNameWithoutExtension
        let p1 = __SOURCE_DIRECTORY__ + "/" + fn + ".png"
        b.Save(p1,Imaging.ImageFormat.Png))


//cI 0.3 0. 1. [|Color.Blue; Color.Yellow; Color.Red|]