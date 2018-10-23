module CaLibCorePgm


// Learn more about F# at http://fsharp.org

open System
open Runs.StatsDynamic

[<EntryPoint>]
let main argv =
  let configFile = argv.[0]
  let rsc = RunConfigs.loadConfig configFile

  Runs.StatsDynamic.start rsc
  //RunConfigs.saveConfig()
  Console.ReadLine() |> ignore
  0 // return an integer exit code
