#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load "..\Utilities\Viz.fs"
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
let fitness (parms:Parm array) = 
    let x = match parms.[0] with F(v,_,_) -> v | _ -> failwith "no match"
    let y = match parms.[1] with F(v,_,_) -> v | _ -> failwith "no match"
    df1 x y

let comparator  = CAUtils.Maximize

let termination step = step.Count > 100
let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness else 0.0

let tk s = s |> Seq.truncate 250 |> Seq.toList

//let kdSimpleCA      = kdSimpleCA fitness comparator parms
let kdWeightedCA    = kdWeightedCA fitness comparator parms
//let kdlWeightedCA   = kdlWeightedCA fitness comparator parms
//let kdGame2PlayerCA = kdGame2PlayerCA fitness comparator parms
//let kdHedonicCA     = kdHedonicCA fitness comparator parms
//let kdIpdCA         = kdIpdCA (0.5,1.9) fitness comparator parms 
let kdIpdCA         = kdIpdCA  (0.2, 0.9) fitness comparator parms 
let kdSchCA         = kdSchelligCA fitness comparator parms 

let m1 = Viz.visualizePopHex Viz.clrKnowledge 512 kdSchCA.Network kdSchCA.Population
Viz.win "m1" m1

Viz.createVid @"D:\repodata\calib\sch.mp4"  512 1000 kdSchCA Viz.clrKnowledge
Viz.createVid @"D:\repodata\calib\wtd.mp4"  512 1000 kdWeightedCA Viz.clrKnowledge

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

