(*
video generation from CA run over a sequence of dynamic landscapes (experimental)
*)
#load "TraceDynamic.fsx"
open CA
open CAUtils
open OpenCvSharp
open Runs.Environment
open Config.Types
open Runs.Types
open System.Drawing

let rsc = 
    {
      SaveFolder    = @"d:\calib\dsst_stats"
      EnvChngSensitivity = [0]
      Restartable   = true
      KDs            = [WTD; IPD; SHS; STK]
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

let  (ShSSt (shStep,_)) = RunStatsDynamic.initSHS EnvChngSensitivity.Insensintive basePop f
let  (WtdSt (wtdStep,_)) = RunStatsDynamic.initWTD EnvChngSensitivity.Insensintive basePop f
let  (StkSt (stkStep,_)) = RunStatsDynamic.initSTK EnvChngSensitivity.Insensintive basePop f
let  (IpdSt (ipdStep,_)) = RunStatsDynamic.initIPD EnvChngSensitivity.Insensintive basePop f


let dSeg ca fSeg indv = Social.segregationAt                        //Schelling-like segregation measure
                            2                                       //radius of neighborhood
                            (1.0 / float Social.ksSegments.Length)  //proportion of each segment or group at start
                            Social.ksSegments                       //list of segments
                            ca                                      //current state of CA
                            fSeg
                            indv


let segClr<'t> fSeg (ca:CA<'t>) (p:Individual<'t>) = 
    let pseg = dSeg ca fSeg p
    let clr = VizUtils.cI pseg 0. 2. [|Color.Blue; Color.Yellow; Color.Red|]
    Scalar.FromRgb(int clr.R, int clr.G, int clr.B) 

let genVizSoc() =
    ////Viz.createVid @"D:\repodata\calib\sch.mp4"  512 1000 kdSchCA Viz.clrKnowledge
    
    //Viz.createVidHeat @"D:\repodata\calib\wtd_seg.mp4"  512 1000 kdWeightedCA (segClr Social.baseSeg)

    let fSegSh = (fun (x:Individual<KDStagHuntStatic.ShKnowledge>) -> Social.ksNum (fst x.KS))
    Viz.createVidHeat @"D:\calib\vids\shnt_seg.mp4" 512 1000 shStep.CA (segClr fSegSh)

    //let fSegIpd = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)
    //Viz.createVidHeat @"D:\repodata\calib\game_seg.mp4"  512 1000 kdIpdCA (segClr fSegIpd)

let dfsnClr<'t> (ca:CA<'t>) (p:Individual<'t>) =
    let dfsn = Social.diffusionAt ca p
    let clr = VizUtils.cI dfsn 0. 1.0 [|Color.Blue; Color.Yellow; Color.Red|]
    Scalar.FromRgb(int clr.R, int clr.G, int clr.B) 


let genVizSocDfsn() =
    ////Viz.createVid @"D:\repodata\calib\sch.mp4"  512 1000 kdSchCA Viz.clrKnowledge
    
    Viz.createVidHeat @"D:\calib\vids\wtd_dfsn.mp4"  512 1000 wtdStep.CA dfsnClr

    Viz.createVidHeat @"D:\calib\vids\shnt_dfsn.mp4" 512 1000 shStep.CA dfsnClr

    Viz.createVidHeat @"D:\calib\vids\game_dfsn.mp4"  512 1000 ipdStep.CA dfsnClr
