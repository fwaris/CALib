#load "TestEnv.fsx"
#load "..\DF1.fs"
#load "ObservableExt.fs"
open CAUtils
open TestEnv
open CA
open DF1

let parms = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

let landscape = 
    "1.01", @"../../Landscapes/test_cone1.01.csv"
//    "2.0", @"../../Landscapes/test_cone2.0.csv"
//    "3.35", @"../../Landscapes/test_cone3.35.csv"
//    "3.5", @"../../Landscapes/test_cone3.5.csv"
//    "3.99", @"../../Landscapes/test_cone3.99.csv"


//2d df1 
let createFtns df (parms:Parm array)  = 
    let x = match parms.[0] with F(v,_,_) -> v | _ -> failwith "no match"
    let y = match parms.[1] with F(v,_,_) -> v | _ -> failwith "no match"
    df x y

let (l,m,fitness) = landscape |>  (fun (l,f)-> let m,d = createDf1 (__SOURCE_DIRECTORY__ + f) in l,m,createFtns d)

let comparator  = CAUtils.Maximize

//let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
let bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
let inline createPop bsp parms init = CAUtils.createPop (init bsp) parms 16 true

let kdIpdCA vmx f c p  = 
    let b = bsp f p c
    let pop = createPop b p CAUtils.baseKsInit |> KDIPDGame.initKS
    let kd = ipdKdist vmx c pop 
    makeCA f c pop b kd KDIPDGame.ipdInfluence

let kdlWeightedCA f c p = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (lWtdMajorityKdist c) CARunner.baseInfluence

let kdWeightedCA f c p  = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (wtdMajorityKdist c) CARunner.baseInfluence

let cts = new System.Threading.CancellationTokenSource()
let obsvblI,fPostI = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsvblK,fPostK= Observable.createObservableAgent<(string*float) seq> cts.Token

let step st = CARunner.step st 2
let vmx = (0.8, 1.5)
let startCA = kdIpdCA vmx fitness comparator parms
//let startCA = kdlWeightedCA fitness comparator parms
let startStep = {CA=startCA; Best=[]; Count=0; Progress=[]}

let run startStep =
    let st = ref startStep
    async {
        while true do
            do! Async.Sleep 250
            st := step !st
            let (_,gb) = best !st
            let gb = gb |> Array.map parmToFloat
            let data =  
                st.Value.CA.Population
                |> Array.map (fun i -> 
                    let p = i.Parms |> Array.map parmToFloat 
                    (p.[0],p.[1]))
                |> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
            let ksCounts =
                st.Value.CA.Population
                |> Seq.map (fun i -> i.KS :> obj)
                |> Seq.collect (
                    function 
                    | :? (Knowledge * Map<Knowledge,float>) as ks -> 
                        let (k,m) = ks
                        List.append [k,1.0] (Map.toList m)
                    | :? Knowledge as k -> [k,1.0]
                    | _-> failwithf "not handled"
                    )
                |> Seq.groupBy (fun (k,f) -> k)
                |> Seq.map (fun (k,fs) -> ks k, fs |> Seq.map snd |> Seq.sum)
                |> Seq.sortBy fst
                |> Seq.toList
            do fPostI data
            do fPostK ksCounts
    }
open FSharp.Charting
open System.Windows.Forms.DataVisualization
let grid = ChartTypes.Grid(Interval=0.1)
let ls = ChartTypes.LabelStyle(TruncatedLabels=true)
LiveChart.FastPoint(obsvblI, Title="Live Pop. Coords.") 
|> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=grid, LabelStyle=ls)
|> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=grid)
|> Chart.WithSeries.DataPoint(Label=l)
;;
LiveChart.Column(obsvblK, Title="Live KS Counts") 
|> Chart.WithStyling(Color=System.Drawing.Color.Chartreuse)
;;

(*
m
Async.Start(run startStep, cts.Token)
cts.Cancel()
*)

(* generate df1 landscape data for excel surface plot

let (mc,df1) = createDf1 (__SOURCE_DIRECTORY__ + snd landscape)
printf "\t"
for y in -1. .. 0.01 .. 1. do printf "%f\t" y
printfn ""
for x in -1. .. 0.01 .. 1. do
    printf "%f\t" x
    for y in -1. .. 0.01 .. 1. do
       printf "%f \t" (df1 x y)
    printfn ""
        
*)

