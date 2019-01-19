module Runs.StatDynamicSeq
open CA
open CAUtils
open Runs.Types
open Runs.Environment
open Runs.Stat
open FSharp.Control

//WTD
let initWTD basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator f
    let pop = basePop |> Array.map (fun (i:Individual<Knowledge>)-> {i with Parms=Array.copy i.Parms })
    let influence = KDWeightedMajority.influence bsp 3
    let ca = makeCA f defaultComparator pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]}
    WtdSt (step,Community.basePrimKs)

//SH
let initSH basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator f
    let pop = basePop |> KDStagHunt.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStagHunt.influence defaultComparator 5 bsp pop
    let ca = makeCA f defaultComparator pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]}
    ShSt (step,Community.fstPrimKs)

//SHS
let initSHS basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator f
    let pop = basePop |> KDStagHuntStatic.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStagHuntStatic.influence None 5 bsp pop
    let ca = makeCA f defaultComparator pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]}
    ShSSt (step,Community.fstPrimKs)

//STK
let initSTK basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator f
    let pop = basePop |> KDStackelberg.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStackelberg.influence defaultComparator pop
    let ca = makeCA f defaultComparator pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]}
    StkSt (step,Community.fstPrimKs)

//IPD
let initIPD basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator f
    let pop = basePop |> KDIPDGame.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = 
        let ada = KDIPDGame.Geometric(0.9,0.01)
        let vmx = (0.2, 0.9)
        KDIPDGame.influence Domain ada vmx defaultComparator pop
    let ca = makeCA f defaultComparator pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]}
    IpdSt (step,Community.gamePrimKs)

let initSteps rsc basePop f =
    [|for kd in rsc.KDs ->
        match kd with
        | WTD -> initWTD basePop f
        | IPD -> initIPD basePop f
        | SH  -> initSH  basePop f
        | SHS -> initSHS basePop f
        | STK -> initSTK basePop f
    |]

//set new fitness function for changed landscape
//reset step counter
//keeps population same
let prepStepsForLandscapeRun ws lndscpCfg =
    lndscpCfg.Steps
    |> Array.map(function 
    | WtdSt (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness := ws.F ; WtdSt(st,f)
    | IpdSt (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness := ws.F ; IpdSt(st,f)
    | ShSt  (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness := ws.F ; ShSt(st,f)
    | ShSSt (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness := ws.F ; ShSSt(st,f)
    | StkSt (st,f) -> let st = {st with Best=[]; Count=0; Progress=[]} in st.CA.Fitness := ws.F ; StkSt(st,f) 
    )

let runSteps envChanged steps =
    steps
    |> Array.map (function 
    | WtdSt (st,f) -> async {return WtdSt(CARunner.step envChanged st defaultMaxBest,f) }
    | IpdSt (st,f) -> async {return IpdSt(CARunner.step envChanged st defaultMaxBest,f) }
    | ShSt  (st,f) -> async {return ShSt(CARunner.step envChanged st defaultMaxBest,f) }
    | ShSSt (st,f) -> async {return ShSSt(CARunner.step envChanged st defaultMaxBest,f) } 
    | StkSt (st,f) -> async {return StkSt(CARunner.step envChanged st defaultMaxBest,f) }
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

let runLandscapeSeq (rsc:RunConfig) lndscpCfg =
    async {
        if lndscpCfg.Landscape >= rsc.NumLandscapes then 
            return None
        else
            let ws = changeEnv lndscpCfg.Ws
            let steps = prepStepsForLandscapeRun ws lndscpCfg
            let lndscpCfg = {lndscpCfg with Ws=ws; EnvCh=true; Steps=steps; Landscape=lndscpCfg.Landscape+1}
            let runResults = AsyncSeq.unfoldAsync (runLandscapeGens rsc) lndscpCfg |> AsyncSeq.toArray
            let stats = runResults |> Array.collect fst
            let lndscpCfg = (Array.last>>snd) runResults
            return Some(stats,lndscpCfg)
    }

let runConfig rsc = 
   asyncSeq {
    for a in rsc.AValues do
    for n in [Hexagon] do
    for i in 1..rsc.Samples do
        let ws = createEnv rsc a
        let f : Fitness = ref ws.F

        let basePop = 
            let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator f
            CAUtils.createPop (baseKsInit bsp) parmDefs rsc.PopulationSize true

        let lndscpCfg = 
            {
                Ws        = ws
                A         = a
                Net       = n
                Landscape = 0
                SampleNum = i
                EnvCh     = true
                Steps     = initSteps rsc basePop f
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