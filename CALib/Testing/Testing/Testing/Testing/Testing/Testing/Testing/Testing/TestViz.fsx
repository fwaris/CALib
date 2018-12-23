﻿#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load "..\Utilities\VizUtils.fs"
#load "..\Utilities\VizNetwork.fs"
#load "..\DF1.fs"
open TestEnv
open CA
open CAUtils
open DF1
open TestEnv
open OpenCvSharp

let parms = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

//let testDf1 = df1_2d CAUtils.rnd.Value 5 (3.,3.) (5.,5.)
//let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone1.01.csv")
//let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone2.0.csv")
//let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone3.35.csv")
//let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone3.5.csv")
let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone3.99.csv")

//2d df1 
let fitness = df1 |> ref

let comparator  = CAUtils.Maximize

let termination step = step.Count > 100
let best stp = if stp.Best.Length > 0 then stp.Best.[0].MFitness else 0.0

let tk s = s |> Seq.truncate 250 |> Seq.toList

let kdWeightedCA    = kdWeightedCA (basePop parms fitness) parms fitness
let kdIpdCA         = kdIpdCA (basePop parms fitness) parms fitness
let kdSchCA         = kdSchelligCA (basePop parms fitness) parms fitness
let kdShCA          = shCA (basePop parms fitness) parms fitness

let testSingle() =
    let m = new Mat(Size(512,512), MatType.CV_8UC3)
    m |> Viz.visualizePopHex 512 Viz.clrKnowledge  kdSchCA.Network kdSchCA.Population
    VizUtils.win "m1" m

let genViz() =
    ////Viz.createVid @"D:\repodata\calib\sch.mp4"  512 1000 kdSchCA Viz.clrKnowledge
    //Viz.createVid @"D:\repodata\calib\wtd.mp4"  512 1000 kdWeightedCA Viz.clrKnowledge

    let shClr ((k,_):KDStagHunt.ShKnowledge) = Viz.brgColors.[Viz.ks k]
    Viz.createVid @"D:\repodata\calib\shnt.mp4" 512 1000 kdShCA shClr

    let ipdClr ((k,_):KDIPDGame.IpdKS) = Viz.brgColors.[Viz.ks k.KS]
    Viz.createVid @"D:\repodata\calib\game.mp4"  512 1000 kdIpdCA ipdClr

(*
let mat = Viz.visualizePopTest 0 512 kdSchCA.Network kdSchCA.Population
let mat2 = Viz.visualizePopTest 255 512 kdSchCA.Network kdSchCA.Population
Viz.win "mat4" mat2
let rL = kdSchCA.Population.Length |> float |> sqrt |> int
let rc xs = xs |> Array.map (fun i -> i / rL, i % rL)
let net59 = kdSchCA.Network kdSchCA.Population 59 |> Array.map (fun p->p.Id) |> Array.append [|59|]
let rc59 = rc net59
let net60 = kdSchCA.Network kdSchCA.Population 60 |> Array.map (fun p->p.Id) |> Array.append [|60|]
let rc60 = rc net60
*)

