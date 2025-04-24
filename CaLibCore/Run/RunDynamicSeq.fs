///Manages experimental run of 
///Cones World landscape sequences with
///mutiple configured KD mechanisms.
///Also saves stats collected during the runs 
module Runs.StatDynamicSeq
open CA
open CAUtils
open Runs.Types
open Config.Types
open Runs.Environment
open Runs.Stat
open FSharp.Control

//WTD
let initWTD envChgSnstvty basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    let pop = basePop |> Array.map (fun (i:Individual<Knowledge>)-> {i with Parms=Array.copy i.Parms })
    let influence = KDWeightedMajority.influence bsp 3
    let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
    let step =  initStep ca
    WtdSt (step,Community.basePrimKs)

//SHS
let initSHS envChgSnstvty basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    let pop = basePop |> KDStagHuntStatic.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStagHuntStatic.influence None 5 bsp pop
    let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
    let step =  initStep ca
    ShSSt (step,Community.fstPrimKs)

//STK
let initSTK envChgSnstvty basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    let pop = basePop |> KDStackelberg.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStackelberg.influence defaultOptKind pop
    let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
    let step =  initStep ca
    StkSt (step,Community.fstPrimKs)

//IPD
let initIPD envChgSnstvty basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    let pop = basePop |> KDIPD.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDIPD.influence defaultOptKind None bsp pop
    let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
    let step =  initStep ca
    IpdSt (step,Community.fstPrimKs)


let initSteps rsc sns basePop f =
    [|for kd in rsc.KDs ->
        match kd with
        | WTD -> initWTD sns basePop f
        | IPD -> initIPD sns basePop f
        | SHS -> initSHS sns basePop f
        | STK -> initSTK sns basePop f
    |]

//set new fitness function for changed landscape
//reset step counter
//keeps population same
let prepStepsForLandscapeRun ws lndscpCfg =
    lndscpCfg.Steps
    |> Array.map(function 
    | WtdSt (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness.Value <- ws.F ; WtdSt(st,f)
    | IpdSt (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness.Value <- ws.F ; IpdSt(st,f)
    | ShSSt (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness.Value <- ws.F ; ShSSt(st,f)
    | StkSt (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness.Value <- ws.F ; StkSt(st,f) 
    | DeSt  (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness.Value <- ws.F ; DeSt(st,f) 
    )

let runSteps envChanged steps =
    steps
    |> Array.map (function 
    | WtdSt (st,f) -> async {return WtdSt(CARunner.step envChanged st defaultMaxBest,f) }
    | IpdSt (st,f) -> async {return IpdSt(CARunner.step envChanged st defaultMaxBest,f) }
    | ShSSt (st,f) -> async {return ShSSt(CARunner.step envChanged st defaultMaxBest,f) } 
    | StkSt (st,f) -> async {return StkSt(CARunner.step envChanged st defaultMaxBest,f) }
    | DeSt (st,f)  -> async {return DeSt(CARunner.step envChanged st defaultMaxBest,f) }
    )
    |> Async.Parallel

let runLandscapeGens rsc lndscpCfg = 
    async {
        if genCount lndscpCfg >= rsc.MaxGen then
            return None
        else
            
            let! steps = lndscpCfg.Steps |> (runSteps lndscpCfg.EnvCh)
            let lndscpCfg = {lndscpCfg with Steps=steps; EnvCh=false}
            let stats = lndscpCfg.Steps |> Array.Parallel.map (statRec rsc lndscpCfg)
            return Some((stats,lndscpCfg),lndscpCfg)
    }

let envChngSnstvy = function 0 -> Insensintive | x -> Every x

let runLandscapeSeq (rsc:RunConfig) lndscpCfg =
    async {
        if lndscpCfg.Landscape >= rsc.NumLandscapes then 
            return None
        else
            let ws = changeEnv lndscpCfg.Ws
            let steps = prepStepsForLandscapeRun ws lndscpCfg
            let lndscpCfg = {lndscpCfg with Ws=ws; EnvCh=true; Steps=steps; Landscape=lndscpCfg.Landscape+1}
            let! runResults = AsyncSeq.unfoldAsync (runLandscapeGens rsc) lndscpCfg |> AsyncSeq.toArrayAsync
            let stats = runResults |> Array.collect fst
            let lndscpCfg = (Array.last>>snd) runResults
            return Some(stats,lndscpCfg)
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
                        let stats = AsyncSeq.unfoldAsync (runLandscapeSeq rsc) lndscpCfg
                        yield! stats
    }
   
let run rsc = 
    let fileName = "Stats.txt"
    initStatFile rsc fileName
    runConfig rsc |> AsyncSeq.iter (Array.iter (writeStats rsc fileName)) |> Async.RunSynchronously
    printfn "done stats"
    //if rsc.Restartable then
    //    Runs.Stat.zipOut rsc