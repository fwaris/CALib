#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load "..\Utilities\VizUtils.fs"
#load "..\Utilities\VizNetwork.fs"
#load "..\DF1.fs"
#load "TestViz.fsx"
open TestEnv
open CA
open CAUtils
open DF1
open TestEnv
open OpenCvSharp
open TestViz
open System.Drawing

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

    let fSegSh = (fun (x:Individual<KDStagHunt.ShKnowledge>) -> Social.ksNum (fst x.KS))
    Viz.createVidHeat @"D:\repodata\calib\shnt_seg.mp4" 512 1000 kdShCA (segClr fSegSh)

    //let fSegIpd = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)
    //Viz.createVidHeat @"D:\repodata\calib\game_seg.mp4"  512 1000 kdIpdCA (segClr fSegIpd)

let dfsnClr<'t> (ca:CA<'t>) (p:Individual<'t>) =
    let dfsn = Social.diffusionAt ca p
    let clr = VizUtils.cI dfsn 0. 1.0 [|Color.Blue; Color.Yellow; Color.Red|]
    Scalar.FromRgb(int clr.R, int clr.G, int clr.B) 


let genVizSocDfsn() =
    ////Viz.createVid @"D:\repodata\calib\sch.mp4"  512 1000 kdSchCA Viz.clrKnowledge
    
    Viz.createVidHeat @"D:\repodata\calib\wtd_dfsn.mp4"  512 1000 kdWeightedCA dfsnClr

    Viz.createVidHeat @"D:\repodata\calib\shnt_dfsn.mp4" 512 1000 kdShCA dfsnClr

    Viz.createVidHeat @"D:\repodata\calib\game_dfsn.mp4"  512 1000 kdIpdCA dfsnClr
