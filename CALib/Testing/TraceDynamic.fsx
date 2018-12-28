#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load @"..\DF1.fs"
#load @"..\Utilities\TraceCharts.fs"
#load @"..\Utilities\VizUtils.fs"
#load "..\Utilities\VizNetwork.fs"
#load @"..\Utilities\VizLandscape.fs"

open CAUtils
open TestEnv
open CA
open DF1
open System.IO
open TraceCharts
open Metrics
open OpenCvSharp
open Tracing
open TraceCharts
open System

let parms = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

let w = createWorld 500 2 (5.,15.) (20., 10.) None None (Some 3.99) |> ref
let m,f = DF1.landscape !w
let fitness = ref f
let maxCone = ref m
let envChangedCount = ref 0

//w.Value.Cones |> Array.map(fun c->c.H) |> Array.sortByDescending (fun x->x)

let initBg = VizLandscape.gen (m,f)
 
let background =
    let f=Path.GetTempFileName()
    initBg.Save f
    f

let kdWeightedCA    = kdWeightedCA (basePop parms fitness) parms fitness
let kdIpdCA         = kdIpdCA (basePop parms fitness) parms fitness
let kdSchCA         = kdSchelligCA (basePop parms fitness) parms fitness
let kdShCA          = shCA (basePop parms fitness) parms fitness
let kdStkCA         = kdStkCA (basePop parms fitness) parms fitness

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

let step envChanged st = CARunner.step envChanged st 2

let fClrIpd ((k,_):KDIPDGame.IpdKS) = Viz.brgColors.[Viz.ks k.KS]
let fClrStk ((k,_):KDStackelberg.StkKnowledge) = Viz.brgColors.[Viz.ks k]
let fClrSh ((k,_):KDStagHunt.ShKnowledge) = Viz.brgColors.[Viz.ks k]

//let startCA,primKS,fClr = kdWeightedCA,KDWeightedMajority.primKS,Viz.clrKnowledge
//let startCA,primKS,fClr = kdIpdCA,KDIPDGame.primKS,fClrIpd
let startCA,primKS,fClr = kdShCA,KDStagHunt.primKS,fClrSh
//let startCA,primKS,fClr = kdStkCA,KDStackelberg.primKS,fClrStk

let startStep = {CA=startCA; Best=[]; Count=0; Progress=[]}

let primarkyKS (x:obj) =
    match x with 
    | :? (KDIPDGame.PrimaryKS * Map<Knowledge,float>) as ks -> (fst ks).KS
    | :? Knowledge as k -> k
    | :? (Knowledge * int) as k -> fst k
    | _-> failwithf "not handled"

let secondaryKS (x:obj) =
    match x with 
    | :? (KDIPDGame.PrimaryKS * Map<Knowledge,float>) as ks -> snd ks |> Some
    | _ -> None

let st = ref startStep

let obsDist,fpDist = Observable.createObservableAgent<float>(Tracing.cts.Token)
let obsMax,fpMax = Observable.createObservableAgent<float>(Tracing.cts.Token) 
let obsMaxCone,fpMaxCone= Observable.createObservableAgent<float>(Tracing.cts.Token)


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
            | :? (Knowledge*int) as k -> [fst k,1.0]
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
    let minDist = st.Value.CA.Population |> Array.map (fun i-> Array.zip i.Parms maxCone.Value.L |> Seq.sumBy (fun (a,b) -> sqr (a - b)) |> sqrt) |> Array.min
    fpMaxCone (maxCone.Value.H)
    if st.Value.Best.IsEmpty |> not then fpMax  st.Value.Best.[0].MFitness
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
    do fpDist minDist; //printfn "d: %f" minDist

let obsMinDist = obsDist |> Observable.withI

let frm = 
  container
      [ 
          chPoints (Some background) "All" obsAll
          chPoints (Some background) "Domain" obsDomain
          chPoints2 (Some background) "Situational" obsSituM
          chPtsLine (Some background) "Normative" obsNormM
          chPoints2 (Some background) "Historical" obsHistM
          chPoints2 (Some background) "Topographical M"  obsTopoM
          chCounts obsKSCounts
          chDisp "Distance" obsMinDist
          //chDisp "Segregation" obsSeg
          chDisp "Diffusion" obsDfsn
      ]

let maxCh = chOne  (chLines (19.5,20.0)  "Max"  [obsMaxCone |> Observable.withI; obsMax |> Observable.withI])


let allCh =  
  let ch =
    chPointsN 
      (Some background) 
      "" 
      ["Domain",obsDomain; "Situational",obsSituational; 
       "Normative",obsNorm; "History",obsHist; 
       "Topographical",obsTopo] 
  chOne ch
  //  |> TraceCharts.containerize
  //let frm = new System.Windows.Forms.Form()
  //frm.Controls.Add(ch)
  //frm.Show()
  //frm

;;
let changeEnvironment() =
    async {
        let w' = updateWorld !w
        let (c,f) = landscape w'
        let bg = VizLandscape.gen (c,f)
        let newBg =
            let f = Path.GetTempFileName()
            bg.Save f
            f
        updateBgForm frm newBg
        updateBgForm allCh newBg
        ///applyBg (Some newBg) (fst allCh) |> ignore
        w := w'
        fitness := f
        maxCone := c
        envChangedCount := !envChangedCount + 1
    }

let ENV_CHANGE_COUNT = 100 //change environment after this many generations

let printPop st =
    let ps = (!st).CA.Population |> Array.take 10
    ps |> Array.iter (fun i -> printfn "%d: %A - %A" i.Id (primKS i.KS) i.Parms)

let ENVCH_MAX = 20

let run startStep =
    let go = ref true
    async {
        while !go do
            do! Async.Sleep 250
            //TODO: detect change from indv performance - change in fitness for same location at different gen
            let envCh =
              if st.Value.Count > 0 && st.Value.Count % ENV_CHANGE_COUNT = 0 && !envChangedCount < ENVCH_MAX then
                changeEnvironment() |> Async.RunSynchronously
                //printf "env changed"
                true
              else 
                false
            //if envCh then printPop st
            st := step envCh !st
            //if envCh then printPop st
            let (bfit,gb) = best !st
            let dist = Array.zip gb maxCone.Value.L |> Seq.sumBy (fun (a,b) -> sqr (a - b)) |> sqrt 
            //fpDist dist
            let solFound = dist < 0.001
            if solFound then 
                if !envChangedCount >= ENVCH_MAX then
                  go := false
                  let m = new Mat(Size(512,512), MatType.CV_8UC3)
                  m |> Viz.visualizePopHex 512 fClr  st.Value.CA.Network st.Value.CA.Population
                  VizUtils.win "m1" m
                  m.SaveImage(@"D:\repodata\calib\run1\game399.png") |> ignore

                //printfn "sol @ %d - B=%A - C=%A" st.Value.Count (bfit,gb) m
            postObs()
    }

;;

let autoStep() = Async.Start(run startStep, cts.Token);;
let singleStep() = st := step false !st; postObs()


(*
autoStep()

singleStep()

cts.Cancel()

changeEnvironment() |> Async.Start

*)
