(*
Cones World, dynamic landscape visualization with
of many different knowledge distribution mechanisms
operating simultaneously

Note: To run this script select all the text in the script and hit Alt-Enter

*)

#load "RunStatsDynamic.fsx"
#load "SetupVideo.fsx"
#load @"..\Utilities\TraceCharts.fs"
#load @"..\Utilities\VizUtils.fs"
#load "..\Utilities\VizNetwork.fs"
#load @"..\Utilities\VizLandscape.fs"

let utc = System.Threading.SynchronizationContext.Current

open CA
open CAUtils
open Runs.Types
open Config.Types
open Runs.Environment
open FSharp.Control
open RunStatsDynamic
open TraceCharts

let rsc = 
    {
      SaveFolder    = @"d:\calib\dsst_stats"
      EnvChngSensitivity = [0]
      Restartable   = true
      //KDs            = [SHS; DE]
      KDs            = [SHS; WTD]
      PopulationSize = 72
      NumCones      = 1000
      RunToMax      = false
      CalcSocMetrics = false
      MaxGen        = 250 //2500
      NumLandscapes = 50
      Samples       = 1
      DistTh        = 0.001
      AValues       = [3.1]
      ChangeHeight  = false
      ChangeRadius  = false
      ChangeLoc     = true
    }

let runConfig rsc = 
    asyncSeq {
        for a in rsc.AValues do
            for n in [Hexagon] do
                for sn in rsc.EnvChngSensitivity do
                    for i in 1..rsc.Samples do
                        let ws = createEnv rsc a
                        let f : Fitness = ref ws.F
                        let basePop = 
                            let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
                            CAUtils.createPop (baseKsInit bsp) parmDefs rsc.PopulationSize true

                        let lndscpCfg = 
                            {
                                Ws                  = ws
                                A                   = a
                                Net                 = n
                                Landscape           = 0
                                SampleNum           = i
                                EnvCh               = true
                                EnvChngSensitivity  = sn
                                Steps               = initSteps rsc (envChngSnstvy sn) basePop f
                            }
                        let stats = runLandscapeSeq rsc lndscpCfg
                        yield! stats
    }


let forms = ref Map.empty

let makeFormsLarge id =
    let o = Tracing.createObservables id 
    let oBg = Some o.obsBkground
    async {
        do! Async.SwitchToContext utc
        let c =
            TraceCharts.container2Col
                [ 
                    chPointsObs "All" oBg o.obsAll
                    chPointsObs "Domain" oBg  o.obsDomain
                    chPointsObs "Situational" oBg o.obsSituational
                    chPointsObs "Normative" oBg  o.obsNorm
                    chPointsObs "Historical" oBg o.obsHist
                    chPointsObs "Topographical M" oBg   o.obsTopo
                    
                    chCounts o.obsKSCounts
                    //chDisp "Distance" o.obsDist
                    //chDisp "Segregation" obsSeg
                    //chDisp "Diffusion" o.obsDfsn
                ]
        c.Text <- id
        //c.Show()

        } |> Async.Start
    let value = o
    forms := forms.Value |> Map.add id value
    value

let makeFormsSmall id =
    let o = Tracing.createObservables id 
    let oBg = Some o.obsBkground
    async {
        do! Async.SwitchToContext utc
        let c =
            TraceCharts.container3Row
                [ 
                    chPointsNwBestObs "All" oBg [
                                                    "Topographic"   , o.obsTopo
                                                    "Normative"     , o.obsNorm
                                                    "History"       , o.obsHist
                                                    "Situational"   , o.obsSituational
                                                    "Domain"        , o.obsDomain]
                                      ("Curr. Best"    , o.obsCurrBest)
                    chCounts o.obsKSCounts

                    chLines (0., 1.)  "Dist to Max" [o.obsDist]

                ]
        c.Text <- id
        //c.Show()

        } |> Async.Start
    let value = o
    forms := forms.Value |> Map.add id value
    value

//set to makeFormsLarge to show each KS separately
let makeForms = makeFormsSmall     

let primarkyKS (x:obj) =
    match x with 
    | :? Knowledge as k -> k
    | :? (Knowledge * int) as k -> fst k
    | _-> failwithf "not handled"

let secondaryKS (x:obj) =
    match x with 
    | _ -> None

let createForms (rsc:RunConfig) =
    rsc.KDs |> List.iter (fun kd -> 
        let id = string kd
        if forms.Value.ContainsKey id |> not then makeForms id |> ignore
    )

let postSteps (gs:GenStats[]) (cfg:LandscapeConfig) = 
    cfg.Steps
    |> Array.zip gs
    |> Array.map (fun (gs,st) -> Runs.Stat.kdId st, st, gs)
    |> Array.iter (fun (id, st, gs) -> 
        if id <> gs.KD then failwithf "mismatch kd stats:%s <> step:%s" id gs.KD
        let o = if forms.Value.ContainsKey id |> not then makeForms id else forms.Value.[id] 
        let dAll =  ksParms None st
        let dDomain = ksParms (Some Domain) st
        let dNorm = ksParms (Some Normative) st                 
        let dHist = ksParms (Some Historical) st
        let dTopo = ksParms (Some Topgraphical) st
        let dSituational = ksParms (Some Situational) st
        let cbest = stepBest st |> List.tryHead |> Option.map (fun m -> m.MParms.[0], m.MParms.[1]) |> Option.defaultValue (0.0,0.0)
        let ksCounts = stepKsCounts st
        let dSeg = gs.Seg
        let minDist =ksParms None st |> Array.map (fun (p1,p2) -> Array.zip [|p1;p2|] cfg.Ws.M.L |> Seq.sumBy (fun (a,b) -> sqr (a - b)) |> sqrt) |> Array.min
        o.fpMaxCone cfg.Ws.M.H 
        let best = stepBest st in if best.IsEmpty |> not then o.fpMax  best.[0].MFitness
        let dfsn = gs.Dffsn
        do o.fpAll dAll
        do o.fpKSCounts ksCounts
        do o.fpDomain dDomain
        do o.fpSituational dSituational
        do o.fpCurrBest [|cbest|]
        
        do o.fpNorm dNorm
        do o.fpHist dHist
        do o.fpTopo dTopo
        do o.fpSeg dSeg
        do o.fpDfsn dfsn
        do o.fpDist minDist; //printfn "d: %f" minDist
        )

let genBg c f =
    let bg = VizLandscape.gen (c,f)
    let f = System.IO.Path.GetTempFileName()
    bg.Save f
    f

let postBg f = forms.Value |> Map.iter(fun k (o) -> 
    async {
        do! Async.SwitchToContext utc
        o.fpBkground (Some f)
    }
    |> Async.Start)



let runner = runConfig rsc |> AsyncSeq.iterAsync(fun x ->
    async {
        do! Async.Sleep 200
        match x with 
        | EnvChange ws -> printfn "Landscape changed"; let f = genBg ws.M ws.F in postBg f
        | LndscpStats (rs,cfg) -> postSteps rs cfg
        })

(*
*)
createForms rsc

async {
    let! r = Async.Catch runner
    match r with
    | Choice1Of2 _ -> ()
    | Choice2Of2 err -> printfn "%A" err
} |> Async.Start

