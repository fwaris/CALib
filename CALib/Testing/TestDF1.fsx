#load "TestEnv.fsx"
#load "..\DF1.fs"
open TestEnv
open CA
open CAUtils
open DF1

let parms = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

//let testDf1 = df1_2d CAUtils.rnd.Value 5 (3.,3.) (5.,5.)
//let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone1.01.csv")
//let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone2.0.csv")
let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone3.35.csv")
//let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone3.5.csv")
//let maxCone,df1 = createDf1 (__SOURCE_DIRECTORY__ + @"../../Landscapes/test_cone3.99.csv")

//2d df1 
let fitness (parms:Parm array) = 
    let x = match parms.[0] with F(v,_,_) -> v | _ -> failwith "no match"
    let y = match parms.[1] with F(v,_,_) -> v | _ -> failwith "no match"
    df1 x y

let comparator  = CAUtils.Maximize

let termination step = step.Count > 100
let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness else 0.0

let tk s = s |> Seq.truncate 1000 |> Seq.toList

//let kdSimpleCA      = kdSimpleCA fitness comparator parms
let kdWeightedCA    = kdWeightedCA fitness comparator parms
let kdlWeightedCA   = kdlWeightedCA fitness comparator parms
//let kdGame2PlayerCA = kdGame2PlayerCA fitness comparator parms
//let kdHedonicCA     = kdHedonicCA fitness comparator parms
//let kdIpdCA         = kdIpdCA (0.5,1.9) fitness comparator parms 
let kdIpdCA         = kdIpdCA (0.7,1.4) fitness comparator parms 

//let kdSimple        = kdSimpleCA |> runCollect dataCollector 2 |> tk
let kdWeigthed      = kdWeightedCA |> runCollect dataCollector 2 |> tk
let kdlWeigthed     = kdlWeightedCA |> runCollect dataCollector 2 |> tk
//let kdGame2Player   = kdGame2PlayerCA |> runCollect dataCollector 2 |> tk
//let kdHedonic       = kdHedonicCA |> runCollect setDataClctr 2 |> tk
let kdIpd           = kdIpdCA |> runCollect ipdDataCollector 2 |> tk
//

let (!!) s = sprintf "%s - %f" s maxCone.H
;;
//plotResults !!"Simple Majority" kdSimple;;
plotResults !!"Weigted Majority" kdWeigthed;;
plotResults !!"Locally Weigted Majority" kdlWeigthed;;
//plotResults !!"Hawk-Dove" kdGame2Player;;
//plotResults !!"Hedonic Game" kdHedonic;;
plotResults !!"IPD Game" kdIpd;;
maxCone;;