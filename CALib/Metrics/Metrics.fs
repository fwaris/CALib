module Metrics
open CA
open System.Threading
open System

//logging and metrics
//any component in the system can post messages
//defined here to log or gather metrics

type Locations = float[] list

type MetricMsg = 
  | TopoState of Locations
  | SitState  of Locations
  | HistState of Locations
  | NormState of Locations
  
let metricsToken = new CancellationTokenSource()
let obsAll,postAll = Observable.createObservableAgent<MetricMsg> metricsToken.Token

#if _LOG_
printfn "compiled with logging enabled"
#else
printfn "not compiled with logging"
#endif