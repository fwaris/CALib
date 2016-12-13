#load "TestEnv.fsx"
#load "..\DF1.fs"
#load "ObservableExt.fs"
open CAUtils
open TestEnv
open CA
open DF1
open System.IO

let parms = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

let landscape = 
//    "1.01", @"../../Landscapes/test_cone1.01.csv"
    "2.0", @"../../Landscapes/test_cone2.0.csv"
//    "3.35", @"../../Landscapes/test_cone3.35.csv"
//    "3.5", @"../../Landscapes/test_cone3.5.csv"
//    "3.99", @"../../Landscapes/test_cone3.99.csv"


//2d df1 
let createFtns df (parms:Parm array)  = 
    let x = match parms.[0] with F(v,_,_) -> v | _ -> failwith "no match"
    let y = match parms.[1] with F(v,_,_) -> v | _ -> failwith "no match"
    df x y

let (l,m,fitness) = landscape |>  (fun (l,f)-> let m,d = createDf1 (__SOURCE_DIRECTORY__ + f) in l,m,createFtns d)

let background = 
    let p = __SOURCE_DIRECTORY__ + (snd landscape) 
    let fn = Path.GetFileNameWithoutExtension(p) + ".png"
    Path.Combine(Path.GetDirectoryName(p),fn)

let comparator  = CAUtils.Maximize

//let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
let bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
let inline createPop bsp parms init = CAUtils.createPop (init bsp) parms 50 true

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
    let ksSet = CAUtils.flatten bsp |> List.map (fun ks->ks.Type) |> set
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (wtdMajorityKdist c ksSet) CARunner.baseInfluence

let cts = new System.Threading.CancellationTokenSource()
let obsvblI,fPostI = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsvblK,fPostK = Observable.createObservableAgent<(string*float) seq> cts.Token
let obsvblD,fPostD = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsvblE,fPostE = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsDsp,fpDsp = Observable.createObservableAgent<int*float> cts.Token

let sqr x = x * x

//dispersion between parms of two individuals
let disp (p1:Parm[]) (p2:Parm[]) =
    let p1 = p1 |> Array.map parmToFloat
    let p2 = p2 |> Array.map parmToFloat
    (0.,p1,p2) |||> Array.fold2 (fun acc p1 p2 -> acc + sqr (p1 - p2))
    |> sqrt

//pop dispersion 
let dispPop (pop:Population<_>) (network:Network<_>) =
    let n = network pop 0
    let esum = 
        (0.,pop) 
        ||> Array.fold (fun acc indv -> 
            (acc,network pop indv.Id) 
            ||> Array.fold(fun acc n -> disp indv.Parms n.Parms))
    let st = esum / float n.Length
    st

let step st = CARunner.step st 2
let vmx = (0.2, 0.9)
//let startCA = kdIpdCA vmx fitness comparator parms
let startCA = kdWeightedCA fitness comparator parms
let startStep = {CA=startCA; Best=[]; Count=0; Progress=[]}

let primarkyKS (x:obj) =
    match x with 
    | :? (Knowledge * Map<Knowledge,float>) as ks -> fst ks
    | :? Knowledge as k -> k
    | _-> failwithf "not handled"

let st = ref startStep

let run startStep =
    let go = ref true
    async {
        while !go do
            do! Async.Sleep 250
            st := step !st
            let (bfit,gb) = best !st
            let gb = gb |> Array.map parmToFloat
            if abs(bfit - m.H) < 0.01 then 
                go := false
                printfn "sol @ %d - B=%A - C=%A" st.Value.Count (bfit,gb) m
            let data =  
                st.Value.CA.Population
                |> Array.map (fun i -> 
                    let p = i.Parms |> Array.map parmToFloat 
                    (p.[0],p.[1]))
                |> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
            let dData =
                st.Value.CA.Population
                |> Array.filter (fun i -> primarkyKS i.KS = Domain)
                |> Array.map (fun i -> 
                    let p = i.Parms |> Array.map parmToFloat 
                    (p.[0],p.[1]))
                |> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
            let eData =
                st.Value.CA.Population
                |> Array.filter (fun i -> primarkyKS i.KS = Situational)// && snd i.KS |> (Map.isEmpty>>not))
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
            do fPostD dData
            do fPostE eData
            do fpDsp (st.Value.Count,dispPop st.Value.CA.Population st.Value.CA.Network)
    }

open FSharp.Charting
open System.Windows.Forms.DataVisualization
let grid = ChartTypes.Grid(Interval=0.1)
let ls = ChartTypes.LabelStyle(TruncatedLabels=true)
LiveChart.FastPoint(obsvblI, Title="Live Pop. Coords.") 
|> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=grid, LabelStyle=ls)
|> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=grid)
|> Chart.WithSeries.DataPoint(Label=l)
|> Chart.WithStyling(Color=System.Drawing.Color.Tomato)
|> withBackground background
;;
LiveChart.FastPoint(obsvblD, Title="Domain") 
|> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=grid, LabelStyle=ls)
|> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=grid)
|> Chart.WithStyling(Color=System.Drawing.Color.Turquoise)
|> Chart.WithSeries.DataPoint(Label=l)
|> withBackground background
;;
LiveChart.FastPoint(obsvblE, Title="Situational") 
|> Chart.WithXAxis(Max=1.0, Min = -1.0, MajorGrid=grid, LabelStyle=ls)
|> Chart.WithYAxis(Max=1.0, Min = -1.0, MajorGrid=grid)
|> Chart.WithStyling(Color=System.Drawing.Color.Chocolate)
|> Chart.WithSeries.DataPoint(Label=l)
;;
LiveChart.Column(obsvblK, Title="Live KS Counts") 
|> Chart.WithStyling(Color=System.Drawing.Color.Tomato)
;;
LiveChart.FastLineIncremental(obsDsp,Title="Dispersion 1.01  IPD")
;;
(*
m
Async.Start(run startStep, cts.Token)
cts.Cancel()
KDIPDGame.KS_ATTRACTION_COFF <- 12.
KDIPDGame.IMPROVE_DEFECT_COFF <- -1.0
TestEnv.defaultNetwork startCA.Population 10
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

//PRNG check for randomness 
[for i in 1 .. 200 do
    for j in 1.. 200 do
        yield (rnd.Value.NextDouble(),rnd.Value.NextDouble())] |> Chart.FastPoint        
*)

