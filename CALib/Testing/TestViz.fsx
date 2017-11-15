#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load "..\Utilities\Viz.fs"
#load "..\DF1.fs"
open TestEnv
open CA
open CAUtils
open DF1
open TestEnv

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

let enc = Viz.encoder @"D:\repodata\calib\m1.mp4" 30. (512,512)
let runCollect data maxBest (ca:CA<Schelling.SchKs>) =
    let m = Viz.visualizePop6 512 ca.Network ca.Population
    for i in 1 .. 10 do enc.Frame m
    m.Release()
    let loop stp = 
        let stp = CARunner.step stp maxBest
        let m = Viz.visualizePop6 512 stp.CA.Network stp.CA.Population
        enc.Frame m
        m.Release()
//        printfn "step %i. fitness=%A" stp.Count (best stp)
//        printfn "KS = %A" (stp.CA.Population |> Seq.countBy (fun x->x.KS))
        stp
    let step = {CA=ca; Best=[]; Count=0; Progress=[]}
    step 
    |> Seq.unfold (fun s -> let s = loop s in (data s,s)  |> Some ) 

let _ = runCollect (fun x->()) 2 kdWeightedCA |> Seq.take 1000 |> Seq.toArray
enc.Release()

//let mat = Viz.visualizePop6 512 kdSchCA.Network kdSchCA.Population
//Viz.win "mat" mat
