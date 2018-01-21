#load "TestEnv.fsx"
#load @"..\DF1.fs"
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

let landscapes = 
    [
        "1.01", @"../../Landscapes/test_cone1.01.csv"
        "2.0", @"../../Landscapes/test_cone2.0.csv"
        "3.35", @"../../Landscapes/test_cone3.35.csv"
        "3.5", @"../../Landscapes/test_cone3.5.csv"
        "3.99", @"../../Landscapes/test_cone3.99.csv"
    ]


//2d df1 
let createFtns df (parms:Parm array)  = 
    let x = match parms.[0] with F(v,_,_) -> v | _ -> failwith "no match"
    let y = match parms.[1] with F(v,_,_) -> v | _ -> failwith "no match"
    df x y

let getFit lndscp = lndscp |>  (fun (l,f)-> let m,d = createDf1 (__SOURCE_DIRECTORY__ + f) in l,m,createFtns d)

let comparator  = CAUtils.Maximize

//let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
let bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
let inline createPop bsp parms init size = CAUtils.createPop (init bsp) parms size true

let kdIpdCA vmx f c p size = 
    let b = bsp f p c
    let pop = createPop b p CAUtils.baseKsInit size |> KDIPDGame.initKS
    let kd = ipdKdist vmx c pop 
    makeCA f c pop b kd KDIPDGame.ipdInfluence

let kdWeightedCA f c p size = 
    let bsp = bsp f p c
    let ksSet = CAUtils.flatten bsp |> List.map (fun ks->ks.Type) |> set
    let pop = createPop bsp p CAUtils.baseKsInit size
    makeCA f c pop bsp (wtdMajorityKdist c ksSet) KDWeightedMajority.wtdMajorityInfluence


let cts = new System.Threading.CancellationTokenSource()
let obsvbl,fPost = Observable.createObservableAgent<(string*string*int*float)> cts.Token

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

let step kd (l,c,st) = 
    let st = CARunner.step st 2
    let disp =  dispPop st.CA.Population st.CA.Network
    fPost (kd,l,st.Count,disp)
    l,c,st

let vmx = (0.2, 0.9)
let ipdCA fitness = kdIpdCA vmx fitness comparator parms 1000
let wtdCA fitness = kdWeightedCA fitness comparator parms 1000
let startStep ca = {CA=ca; Best=[]; Count=0; Progress=[]}

let primarkyKS (x:obj) =
    match x with 
    | :? (KDIPDGame.PrimaryKS * Map<Knowledge,float>) as ks -> (fst ks).KS
    | :? Knowledge as k -> k
    | _-> failwithf "not handled"

let runLandscapes() =
    let fits = landscapes |> List.map getFit 
    let ipdCAs = fits |> List.map (fun (l,c,f) ->l,c, startStep (ipdCA f))
    let wtdCAs = fits |> List.map (fun (l,c,f) ->l,c, startStep (wtdCA f))
    async {
        let runs =
            ((ipdCAs,wtdCAs), [for i in 0 .. 75 -> i]) 
            ||> List.fold(fun (ipds,wtds) i ->
                ipds |> List.map (step "ipd"),
                wtds |> List.map (step "wtd")
            )
        return ()
    }

open FSharp.Charting
open System.Windows.Forms.DataVisualization
let grid = ChartTypes.Grid(Interval=0.1)
let ls = ChartTypes.LabelStyle(TruncatedLabels=true)

for i in ["ipd";"wtd"] do
    for (l,_) in landscapes do
        let obs = 
            obsvbl 
            |>  Observable.filter (fun (kd,ls,_,_) ->kd=i && ls=l) 
            |> Observable.map (fun ((_,_,i,d) as x) -> i,d)
        LiveChart.FastLineIncremental(
            obs, 
            Title=sprintf "Dispersion Index %s %s" i l)
            .ShowChart() |> ignore
(*
Async.Start(runLandscapes(), cts.Token)
cts.Cancel()
*)


