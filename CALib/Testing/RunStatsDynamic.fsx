#load "SetupEnv.fsx"
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
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0}
    WtdSt (step,Community.basePrimKs)

//SH
let initSH envChgSnstvty basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    let pop = basePop |> KDStagHunt.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStagHunt.influence defaultOptKind 5 bsp pop
    let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0}
    ShSt (step,Community.fstPrimKs)

//SHS
let initSHS envChgSnstvty basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    let pop = basePop |> KDStagHuntStatic.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStagHuntStatic.influence None 5 bsp pop
    let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0}
    ShSSt (step,Community.fstPrimKs)

//STK
let initSTK envChgSnstvty basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    let pop = basePop |> KDStackelberg.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStackelberg.influence defaultOptKind pop
    let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0}
    StkSt (step,Community.fstPrimKs)

//IPD
let initIPD envChgSnstvty basePop f = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    let pop = basePop |> KDIPD.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDIPD.influence defaultOptKind None bsp pop
    let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
    let step =  {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0}
    IpdSt (step,Community.fstPrimKs)

let initSteps rsc sns basePop f =
    [|for kd in rsc.KDs ->
        match kd with
        | WTD -> initWTD sns basePop f
        | IPD -> initIPD sns basePop f
        | SH  -> initSH  sns basePop f
        | SHS -> initSHS sns basePop f
        | STK -> initSTK sns basePop f
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


let parms (pop:Population<_>) = 
    pop |> Array.map (fun i -> 
        let p = i.Parms 
        (p.[0],p.[1]))


let ksParms ks = function
    | WtdSt (st,f) -> match ks with Some ks ->  st.CA.Population |> Array.filter (fun x->f x.KS = ks) |> parms | None -> parms st.CA.Population
    | IpdSt (st,f) -> match ks with Some ks ->  st.CA.Population |> Array.filter (fun x->f x.KS = ks) |> parms | None -> parms st.CA.Population
    | ShSt  (st,f) -> match ks with Some ks ->  st.CA.Population |> Array.filter (fun x->f x.KS = ks) |> parms | None -> parms st.CA.Population
    | ShSSt (st,f) -> match ks with Some ks ->  st.CA.Population |> Array.filter (fun x->f x.KS = ks) |> parms | None -> parms st.CA.Population
    | StkSt (st,f) -> match ks with Some ks ->  st.CA.Population |> Array.filter (fun x->f x.KS = ks) |> parms | None -> parms st.CA.Population

let stepBest = function
    | WtdSt (st,f) -> st.Best
    | IpdSt (st,f) -> st.Best
    | ShSt  (st,f) -> st.Best
    | ShSSt (st,f) -> st.Best
    | StkSt (st,f) -> st.Best

let colors = 
        [
        255,255,0
        255,255,224
        255,250,205
        250,250,210
        255,239,213
        255,228,181
        255,218,185
        238,232,170
        240,230,140
        189,183,107
        255,215,0
        ]

let toColor (r,g,b) = System.Drawing.Color.FromArgb(1,r,g,b)

let ksName = function 
    | Domain        -> "Domain" 
    | Historical    -> "Historical" 
    | Situational   -> "Situational" 
    | Normative     -> "Normative" 
    | Topgraphical  -> "Topographical"
    | _             -> failwith "shoud not happen"


let ksCounts (pop:Population<_>) =
    pop
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
    |> Seq.map (fun (k,fs) -> ksName k, fs |> Seq.map snd |> Seq.sum)
    |> Seq.sortBy fst
    |> Seq.toList

let stepKsCounts = function
    | WtdSt (st,f) -> ksCounts st.CA.Population 
    | IpdSt (st,f) -> ksCounts st.CA.Population 
    | ShSt  (st,f) -> ksCounts st.CA.Population 
    | ShSSt (st,f) -> ksCounts st.CA.Population 
    | StkSt (st,f) -> ksCounts st.CA.Population 


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

type EvalResult = EnvChange of WorldState | LndscpStats of (GenStats[]*LandscapeConfig)

let rec runLandscapeSeq (rsc:RunConfig) lndscpCfg =
    asyncSeq {
        if lndscpCfg.Landscape >= rsc.NumLandscapes then 
            ()
        else
            let ws = changeEnv lndscpCfg.Ws
            yield EnvChange ws
            let steps = prepStepsForLandscapeRun ws lndscpCfg
            let lndscpCfg = {lndscpCfg with Ws=ws; EnvCh=true; Steps=steps; Landscape=lndscpCfg.Landscape+1}
            let runResults = AsyncSeq.unfoldAsync (runLandscapeGens rsc) lndscpCfg |> AsyncSeq.map LndscpStats
            yield! runResults
            yield! runLandscapeSeq rsc lndscpCfg
    }



//let runConfig rsc = 
//    asyncSeq {
//        for a in rsc.AValues do
//            MetaLrn.Dbg.A <- a
//            for n in [Hexagon] do
//                for sn in rsc.EnvChngSensitivity do
//                    for i in 1..rsc.Samples do
//                        let ws = createEnv rsc a
//                        let f : Fitness = ref ws.F

//                        let basePop = 
//                            let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
//                            CAUtils.createPop (baseKsInit bsp) parmDefs rsc.PopulationSize true

//                        let lndscpCfg = 
//                            {
//                                Ws                  = ws
//                                A                   = a
//                                Net                 = n
//                                Landscape           = 0
//                                SampleNum           = i
//                                EnvCh               = true
//                                EnvChngSensitivity  = sn
//                                Steps               = initSteps rsc (envChngSnstvy sn) basePop f
//                            }
//                        let stats = AsyncSeq.unfoldAsync (runLandscapeSeq rsc) lndscpCfg
//                        yield! stats
//    }
   
//let run rsc = 
//    let fileName = "Stats.txt"
//    initStatFile rsc fileName
//    runConfig rsc |> AsyncSeq.iter (Array.iter (writeStats rsc fileName)) |> Async.RunSynchronously
//    printfn "done stats"
//    //if rsc.Restartable then
//    //    Runs.Stat.zipOut rsc
