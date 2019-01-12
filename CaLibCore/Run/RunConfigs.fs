module RunConfigs
open MBrace.FsPickler
open Runs.Types
open System.IO
open System

let configToSave = 
    {
      SaveFolder    = @"D:\repodata\calib\dsst_stats"
      KDs            = [WTD; IPD; SH; STK]
      PopulationSize = 72
      NumCones      = 500
      RunToMax      = false
      CalcSocMetrics = false
      MaxGen        = 250
      NumLandscapes = 50
      Samples       = 2
      DistTh        = 0.001
      AValues       = [1.0; 3.0;3.3; 3.6; 3.8; 3.9]
      ChangeHeight  = false
      ChangeRadius  = false
      ChangeLoc     = true
    }

let saveConfig() =
  let ser = FsPickler.CreateXmlSerializer(indent=true)
  use f = File.CreateText("Config.xml")
  ser.Serialize(f,configToSave)

let loadConfig file =
  use f = File.OpenText file
  let ser = FsPickler.CreateXmlSerializer(indent=true)
  let config:RunConfig = ser.Deserialize(f)
  config

let createJobs() =
    let folder = @"D:\repodata\calib\jobs"
    if Directory.Exists folder |> not then Directory.CreateDirectory folder |> ignore
    //let kds = [WTD; IPD; SH; STK]
    let kds = [SH]                              //***** limited kd
    let avals = [1.0; 3.6; 3.9]
    //let kdav = seq {for kd in kds do  
    //                    for av in avals do
    //                        for s in 1..30 do
    //                            yield kd,av,s}
    let kdav = seq {for av in avals do
                            for s in 1..100 do
                                yield kds,av,s}
    let ser = FsPickler.CreateXmlSerializer(indent=true)
    let saveFolder = "/wsu/home/ar/ar86/ar8623/calib/jobout"
    kdav |> Seq.iteri (fun i (k,a,s) -> 
        let fnJob = Path.Combine(folder,sprintf "job_%d.xml" i)
        //let fnOut = sprintf "%s/%A_%A_%d" saveFolder k a s
        let fnOut = sprintf "%s/KD_%A_%d" saveFolder a s
        let cfg = 
             {
                  SaveFolder    = fnOut
                  KDs            = k
                  PopulationSize = 36
                  NumCones      = 1000
                  RunToMax      = true
                  CalcSocMetrics = true
                  MaxGen        = 250
                  NumLandscapes = 100
                  Samples       = 1
                  DistTh        = 0.001
                  AValues       = [a]
                  ChangeHeight  = false
                  ChangeRadius  = false
                  ChangeLoc     = true
            }
        use f = File.CreateText(fnJob)
        ser.Serialize(f,cfg)
   )
