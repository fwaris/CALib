module CaLibCorePgm


// Learn more about F# at http://fsharp.org

open System
open Runs.StatsDynamic
open System.IO

type RunCommand = Job of string | Cfg of string | NoArgs | InvalidArgs

let (|Job|Cfg|NoArgs|InvalidArgs|) = function
    | [||]          -> NoArgs
    | [|"-j"; job|] -> Job job
    | [|loc|]       -> Cfg loc
    | _             -> InvalidArgs

let PBS_ARRAY_INDEX = "PBS_ARRAY_INDEX"

let b2o s c = if c then Some s else None //boolean-to-option

let runJob jobLoc =
    (Directory.Exists jobLoc) |> b2o jobLoc
    |> function 
    | None -> printfn "Jobs folder '%s' not found" jobLoc; 1
    | Some j -> 
        let fileId = System.Environment.GetEnvironmentVariable(PBS_ARRAY_INDEX)
        (fileId <> null) |> b2o (j,fileId)
        |> function 
        | None -> printfn "Environment variable '%s' not found" PBS_ARRAY_INDEX; 1
        | Some (j,f) ->
            let configFile = Path.Combine(j,"job_" + f + ".xml")
            (File.Exists configFile) |> b2o configFile
            |> function
            | None -> printfn "Config file '%s' not found" configFile; 1
            | Some f -> 
                let rsc = RunConfigs.loadConfig f
                Runs.StatsDynamic.start rsc |> Async.RunSynchronously
                0


[<EntryPoint>]
let main argv =
    match argv with
    | NoArgs | InvalidArgs ->
        printfn "usage: configFile | -j jobsFolder"
        printfn "config file missing sample saved" 
        RunConfigs.saveConfig()
        RunConfigs.createJobs()
        1
    | Cfg (loc) -> 
        let rsc = RunConfigs.loadConfig loc
        Runs.StatsDynamic.start rsc |> Async.RunSynchronously
        0
    | Job (loc) -> 
        runJob loc
