module MetaLrnKdo

open Microsoft.ML
open Microsoft.ML.Sweeper
open Microsoft.ML.Sweeper.Algorithms
open System


let createGenerator gen =
    { new IComponentFactory<IValueGenerator> with
        member x.CreateComponent(ctx) = gen
    }

let createSweeper gen =
    { new IComponentFactory<ISweeper> with
        member x.CreateComponent(ctx) = gen
    }

let initSweeperKdo ctx  (parms:IValueGenerator[])  = 
    let args = KdoSweeper.Arguments()
    args.SweptParameters <- parms |> Array.map(createGenerator)
    //args.HistoryLength <- maxHistory + 1                       
    //args.NumberInitialPopulation <- maxHistory + 1 
    let baseSweeper = KdoSweeper(ctx,args)
    baseSweeper

//hyperparameter names
let p_start = "start"
let p_end = "end"
let p_decay = "decay"

let hyperParameters : IValueGenerator[] = 
    [|
        //FloatParamArguments (Name=p_start, Min=1.0f, Max=1.25f, StepSize=Nullable 0.1 )
        //FloatParamArguments (Name=p_end, Min=0.7f, Max=0.8f, StepSize=Nullable 0.1)
        //FloatParamArguments (Name=p_decay, Min=0.97f, Max=0.98f, StepSize=Nullable 0.01)

        FloatParamArguments (Name=p_start, Min=1.0f, Max=1.5f, StepSize=Nullable 0.1 )
        FloatParamArguments (Name=p_end, Min=0.6f, Max=0.9f, StepSize=Nullable 0.1)
        FloatParamArguments (Name=p_decay, Min=0.96f, Max=0.999f, StepSize=Nullable 0.01)
    |]
    |> Array.map (fun x -> FloatValueGenerator(x) :> _) 

let checkNewBest bestM (rr:RunResult) rs =
    match rr.IsMetricMaximizing, rr.MetricValue, !bestM with
    | _,n,None                   -> bestM := Some n
    | true,n, Some o when n > o  -> bestM := Some n; printfn "new max %s" rs
    | false,n,Some o when n < o  -> bestM := Some n; printfn "new min %s" rs
    | _                          -> ()

let private ctx = MLContext(Nullable 10)
let sweeper = initSweeperKdo ctx hyperParameters 

module Dbg =
    open System.IO
    open System
    let outPath = @"D:\calib\dsst_stats\kdo.txt"
    let mutable A = 1.0

    let reset() =
        if File.Exists outPath then File.Delete outPath |> ignore
   
    let log (pStart:float) (pEnd:float) (decay:float) (result:float) =
        let wrtHdr = File.Exists outPath
        use str = File.AppendText outPath
        if wrtHdr |> not then 
            str.WriteLine(String.Join("\t","Start","End","Decay","Result"))
        let ln = String.Join("\t", pStart,pEnd,decay,result)
        str.WriteLine(ln)
