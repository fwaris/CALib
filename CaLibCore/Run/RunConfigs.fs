module RunConfigs
open MBrace.FsPickler
open Runs.StatsDynamic
open System.IO

let configToSave = 
    {
      SaveFolder    = @"D:\repodata\calib\dsst_stats"
      RunToMax      = true
      CalcSocMetrics = true
      MaxGen        = 250
      NumLandscapes = 50
      Samples       = 30
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
  let config:RunsConfig = ser.Deserialize(f)
  config