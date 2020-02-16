#load "TraceDynamic.fsx"
open CA
open CAUtils
open OpenCvSharp
open Runs.Environment
open Config.Types
open Runs.Types


let rsc = 
    {
      SaveFolder    = @"d:\calib\dsst_stats"
      EnvChngSensitivity = [0]
      Restartable   = true
      KDs            = [WTD; IPD; SHS; STK]
      PopulationSize = 360
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

let ws = createEnv rsc 3.1
let f : Fitness = ref ws.F

let basePop = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    CAUtils.createPop (baseKsInit bsp) parmDefs rsc.PopulationSize true

let  (ShSSt (stepShs,primKsShs)) = RunStatsDynamic.initSHS EnvChngSensitivity.Insensintive basePop f
let  (IpdSt (stepIpd,primKsIpd)) = RunStatsDynamic.initIPD EnvChngSensitivity.Insensintive basePop f
let  (WtdSt (stepWtd,primKsWtd)) = RunStatsDynamic.initWTD EnvChngSensitivity.Insensintive basePop f


let testSingle() =
    let m = new Mat(Size(512,512), MatType.CV_8UC3)
    m |> Viz.visualizePopHex 512 (primKsShs>>Viz.clrKnowledge)  stepShs.CA.Network stepShs.CA.Population
    VizUtils.win "m1" m

let genViz() =

    Viz.createVid @"D:\calib\vids_large\shs.mp4" 512 1000 stepShs.CA (primKsShs>>Viz.clrKnowledge)
    Viz.createVid @"D:\calib\vids_large\ipd.mp4" 512 1000 stepIpd.CA (primKsIpd>>Viz.clrKnowledge)
    Viz.createVid @"D:\calib\vids_large\wtd.mp4" 512 1000 stepWtd.CA (primKsWtd>>Viz.clrKnowledge)

(*   older ipd version

#load @"C:\Users\fwaris\Downloads\CALib-pretrim\CALib-pretrim\CALib\KnowledgeDistribution\KDIPDGame.fs"
let genVizOldIpd()  =

    let influence pop = 
        let ada = KDIPDGame.Geometric(0.9,0.01)
        let vmx = (0.2, 0.9)
        KDIPDGame.influence Domain ada vmx defaultOptKind pop


    let initIPD2 envChgSnstvty basePop f = 
        let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
        let pop = basePop |> KDIPDGame.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })

        let influence = influence pop
        let ca = makeCA f envChgSnstvty defaultOptKind pop bsp influence defaultNetwork
        let step =  {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0}
        (step,KDIPDGame.primKS)

    let stepIpd2,pksIpd2 = initIPD2 EnvChngSensitivity.Insensintive basePop f
    Viz.createVid @"D:\calib\vids_large\ipd2.mp4" 512 1000 stepIpd2.CA (pksIpd2>>Viz.clrKnowledge)
*)
    
