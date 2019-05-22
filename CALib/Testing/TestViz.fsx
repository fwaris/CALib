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
      KDs            = [WTD; IPD; SH; STK]
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

let ws = createEnv rsc 3.1
let f : Fitness = ref ws.F

let basePop = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultOptKind f
    CAUtils.createPop (baseKsInit bsp) parmDefs rsc.PopulationSize true

let  (ShSSt (step,primKs)) = RunStatsDynamic.initSHS EnvChngSensitivity.Insensintive basePop f

let testSingle() =
    let m = new Mat(Size(512,512), MatType.CV_8UC3)
    m |> Viz.visualizePopHex 512 (primKs>>Viz.clrKnowledge)  step.CA.Network step.CA.Population
    VizUtils.win "m1" m

let genViz() =

    Viz.createVid @"D:\calib\vids\shnt.mp4" 512 1000 step.CA (primKs>>Viz.clrKnowledge)

