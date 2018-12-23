#load "TestEnv.fsx"
#load @"..\DF1.fs"
#load @"..\Utilities\TraceCharts.fs"
#load "SetupVideo.fsx"
#load @"..\Utilities\VizUtils.fs"
#load @"..\Utilities\VizLandscape.fs"

open CAUtils
open TestEnv
open CA
open DF1
open System.IO
open Tracing
open TraceCharts

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
    //"3.99", @"../../Landscapes/test_cone3.99.csv"


//2d df1 
let createFtns df (parms:float array)  = 
    df parms

let readFile = Some  @"D:\repodata\calib\dynstats\Env_IPD_131652648800521969.env"

let (l,m,fitness),background = 
  match readFile with
  | Some file ->
    let w1 = loadEnv file
    let m,f = DF1.landscape w1
    let ls = "file",m,ref f
    let bmp = VizLandscape.gen (m,f)
    let bg=Path.GetTempFileName()
    bmp.Save bg
    ls,bg
  | None ->
   let bg = 
      let p = __SOURCE_DIRECTORY__ + (snd landscape) 
      let fn = Path.GetFileNameWithoutExtension(p) + ".png"
      Path.Combine(Path.GetDirectoryName(p),fn)     
   let ls = landscape |>  (fun (l,f)-> let m,d = createDf1 (__SOURCE_DIRECTORY__ + f) in l,m,ref (createFtns d))
   ls,bg

let kdWeightedCA    = kdWeightedCA (basePop parms fitness) parms fitness
let kdIpdCA         = kdIpdCA (basePop parms fitness) parms fitness
let kdSchCA         = kdSchelligCA (basePop parms fitness) parms fitness
let kdShCA          = shCA (basePop parms fitness) parms fitness
let kdStkCA         = kdStkCA (basePop parms fitness) parms fitness

//let startCA,primKS = kdWeightedCA,KDWeightedMajority.primKS
//let startCA,primKS = kdIpdCA,KDIPDGame.primKS
//let startCA,primKS = kdShCA,KDStagHunt.primKS
let startCA,primKS = kdStkCA,KDStackelberg.primKS

let inline sqr x = x * x

////dispersion between parms of two individuals
//let disp (p1:float[]) (p2:float[]) =
//    (0.,p1,p2) |||> Array.fold2 (fun acc p1 p2 -> acc + sqr (p1 - p2))
//    |> sqrt

////pop dispersion 
//let dispPop (pop:Population<_>) (network:Network<_>) =
//    let n = network pop 0
//    let esum = 
//        (0.,pop) 
//        ||> Array.fold (fun acc indv -> 
//            (acc,network pop indv.Id) 
//            ||> Array.fold(fun acc n -> disp indv.Parms n.Parms))
//    let st = esum / float n.Length
//    st


let step st = CARunner.step false st 2
let startStep = {CA=startCA; Best=[]; Count=0; Progress=[]}

let primarkyKS (x:obj) =
    match x with 
    | :? (KDIPDGame.PrimaryKS * Map<Knowledge,float>) as ks -> (fst ks).KS
    | :? Knowledge as k -> k
    | :? (Knowledge*int) as k -> fst k
    | _-> failwithf "not handled"

let secondaryKS (x:obj) =
    match x with 
    | :? (KDIPDGame.PrimaryKS * Map<Knowledge,float>) as ks -> snd ks |> Some
    | :? Knowledge as k -> None
    | :? (Knowledge*int) -> None
    | _-> failwithf "not handled"

let st = ref startStep

let postObs() = 
    let dAll =  
        st.Value.CA.Population
        |> Array.map (fun i -> 
            let p = i.Parms 
            (p.[0],p.[1]))
        //|> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
    let dDomain =
        st.Value.CA.Population
        |> Array.filter (fun i -> primarkyKS i.KS = Domain)
        |> Array.map (fun i -> 
            let p = i.Parms 
            (p.[0],p.[1]))
        //|> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
    let dSituational =
        st.Value.CA.Population
        |> Array.filter (fun i -> primarkyKS i.KS = Situational)// && snd i.KS |> (Map.isEmpty>>not))
        |> Array.map (fun i -> 
            let p = i.Parms  
            (p.[0],p.[1]))
        //|> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
    let dNorm =
        st.Value.CA.Population
        |> Array.filter (fun i -> primarkyKS i.KS = Normative)// && snd i.KS |> (Map.isEmpty>>not))
        |> Array.map (fun i -> 
            let p = i.Parms 
            (p.[0],p.[1]))
        //|> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
    let dHist =
        st.Value.CA.Population
        |> Array.filter (fun i -> primarkyKS i.KS = Historical)// && snd i.KS |> (Map.isEmpty>>not))
        |> Array.map (fun i -> 
            let p = i.Parms 
            (p.[0],p.[1]))
        //|> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
    let dTopo =
        st.Value.CA.Population
        |> Array.filter (fun i -> primarkyKS i.KS = Topgraphical)// && snd i.KS |> (Map.isEmpty>>not))
        |> Array.map (fun i -> 
            let p = i.Parms 
            (p.[0],p.[1]))
        //|> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
    let dSecDmn =
        st.Value.CA.Population
        |> Array.filter (fun i -> secondaryKS i.KS |> Option.exists (fun m->m.ContainsKey Domain))// && snd i.KS |> (Map.isEmpty>>not))
        |> Array.map (fun i -> 
            let p = i.Parms 
            (p.[0],p.[1]))
        //|> Array.append [|-1.,-1.;(1.,1.);1.,-1.;-1.,1.;gb.[0],gb.[1]|]
    let ksCounts =
        st.Value.CA.Population
        |> Seq.map (fun i -> i.KS :> obj)
        |> Seq.collect (
            function 
            | :? (KDIPDGame.PrimaryKS * Map<Knowledge,float>) as ks -> 
                let (pk,m) = ks
                let k = pk.KS
                let lvl = pk.Level
                List.append [k,lvl] (Map.toList m)
            | :? Knowledge as k -> [k,1.0]
            | :? (Knowledge*int) as k -> [fst k, 1.0]
            | _-> failwithf "not handled"
            )
        |> Seq.groupBy (fun (k,f) -> k)
        |> Seq.map (fun (k,fs) -> ks k, fs |> Seq.map snd |> Seq.sum)
        |> Seq.sortBy fst
        |> Seq.toList
    let dSeg = Social.segregation                                   //Schelling-like segregation measure
                  2                                                 //radius of neighborhood
                  (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
                  Social.ksSegments                                 //list of segments
                  st.Value.CA                                       //current state of CA
                  (segF primKS)
    let dfsn = Social.diffusion st.Value.CA
    do fpAll dAll
    do fpKSCounts ksCounts
    do fpDomain dDomain
    do fpSituational dSituational
    do fpSecDmn dSecDmn
    do fpNorm dNorm
    do fpHist dHist
    do fpTopo dTopo
    do fpDispersion (st.Value.Count,Social.diffusion st.Value.CA)
    do fpSeg dSeg
    do fbDfsn dfsn

let run startStep =
    let go = ref true
    async {
        while !go do
            do! Async.Sleep 250
            st := step !st
            let (bfit,gb) = best !st
            if abs(bfit - m.H) < 0.01 then 
                go := false
                printfn "sol @ %d - B=%A - C=%A" st.Value.Count (bfit,gb) m
            postObs()
    }

;;

container
    [ 
        chPoints (Some background) "All" obsAll
        chPoints (Some background) "Domain" obsDomain
        chPoints2 (Some background) "Situational" obsSituM
        chPtsLine (Some background) "Normative" obsNormM
        chPoints2 (Some background) "Historical" obsHistM
        chPoints2 (Some background) "Topographical M"  obsTopoM
        chCounts obsKSCounts
        chDisp "Segregation" obsSeg
        chDisp "Diffusion" obsDfsn
    ]

let autoStep() = Async.Start(run startStep, cts.Token);;
let singleStep() = st := step !st; postObs()
(*
autoStep()

singleStep()

cts.Cancel()


chDisp "KS Counts" obsDispersion
chPoints None "Ind with Secondary Domain" obsSecDmn
*)

(*
m
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

