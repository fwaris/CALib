module RunConfigs
open MBrace.FsPickler
open Config.Types
open System.IO

let configToSave = 
    {
      SaveFolder    = @"d:\calib\dsst_stats"
      EnvChngSensitivity = [0; 1; 5; 10]
      Restartable   = true
      KDs            = [WTD; IPD; SH; STK]
      PopulationSize = 360
      NumCones      = 1000
      RunToMax      = false
      CalcSocMetrics = false
      MaxGen        = 250
      NumLandscapes = 50
      Samples       = 2
      DistTh        = 0.001
      AValues       = [1.0; 3.1; 3.6; 3.9]
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

let serializeConfig (ser:XmlSerializer) folder i (cfg:RunConfig) =
    let fnJob = Path.Combine(folder,sprintf "job_%d.xml" i)
    use f = File.CreateText(fnJob)
    ser.Serialize(f,cfg)

let createJobs() =
    let folder = @"d:\calib\jobs"
    if Directory.Exists folder |> not then Directory.CreateDirectory folder |> ignore
    let kds = [WTD; IPD; SHS; STK]
    //let kds = [SH; SHS]                              //***** limited kd
    let avals = [1.0; 3.1; 3.6; 3.9]
    let NUM_SAMPLES = 200
    //let envChSens = [0; 1; 5; 10]
    let envChSens = [0; 1]
    //let kdav = seq {for kd in kds do  
    //                    for av in avals do
    //                        for s in 1..30 do
    //                            yield kd,av,s}
    let kdav = seq {for av in avals do
                            for s in 1..NUM_SAMPLES do
                                for sn in envChSens do
                                    yield kds,av,s,sn}
    let ser = FsPickler.CreateXmlSerializer(indent=true)
    let saveFolder = "/wsu/home/ar/ar86/ar8623/calib/jobout_sh"
    kdav |> Seq.iteri (fun i (k,a,s,sn) -> 
        let fnJob = Path.Combine(folder,sprintf "job_%d.xml" i)
        //let fnOut = sprintf "%s/%A_%A_%d" saveFolder k a s
        let fnOut = sprintf "%s/KD_%A_%d_%d" saveFolder a s sn
        let cfg = 
             {
                  SaveFolder    = fnOut
                  EnvChngSensitivity = [sn]
                  Restartable   = true
                  KDs            = k
                  PopulationSize = 36
                  NumCones      = 1000
                  RunToMax      = true
                  CalcSocMetrics = true
                  MaxGen        = 2500
                  NumLandscapes = 50
                  Samples       = 1
                  DistTh        = 0.001
                  AValues       = [a]
                  ChangeHeight  = true
                  ChangeRadius  = false
                  ChangeLoc     = false
            }
        serializeConfig ser folder i cfg
   )

 (*  //amped environment 
             {
                  SaveFolder    = fnOut
                  Restartable   = true
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
*)
(*base environment 
             {
                  SaveFolder    = fnOut
                  KDs            = k
                  PopulationSize = 72
                  NumCones      = 500
                  RunToMax      = true
                  CalcSocMetrics = true
                  MaxGen        = 250
                  NumLandscapes = 50
                  Samples       = 1
                  DistTh        = 0.001
                  AValues       = [a]
                  ChangeHeight  = false
                  ChangeRadius  = false
                  ChangeLoc     = true
            }
*)