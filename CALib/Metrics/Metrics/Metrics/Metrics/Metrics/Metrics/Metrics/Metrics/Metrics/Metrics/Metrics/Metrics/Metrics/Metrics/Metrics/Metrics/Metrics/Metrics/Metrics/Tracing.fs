module Tracing

let cts = new System.Threading.CancellationTokenSource()
let obsAll,fpAll = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsKSCounts,fpKSCounts = Observable.createObservableAgent<(string*float) seq> cts.Token
let obsDomain,fpDomain = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsSecDmn,fpSecDmn = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsSituational,fpSituational = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsNorm,fpNorm = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsHist,fpHist = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsTopo,fpTopo = Observable.createObservableAgent<(float*float) seq> cts.Token
let obsDispersion,fpDispersion = Observable.createObservableAgent<int*float> cts.Token
let obsSeg_,fpSeg = Observable.createObservableAgent<float> cts.Token
let obsSeg = obsSeg_ |> Observable.withI
let obsDfsn_,fbDfsn= Observable.createObservableAgent<float> cts.Token
let obsDfsn = obsDfsn_ |> Observable.withI

//KS-specific internal states
let obsTopoM = 
  Metrics.obsAll 
  |> Observable.choose (function Metrics.TopoState s -> Some s | _ -> None)
  |> Observable.map (fun l ->  l |> List.toSeq |> Seq.map (fun f -> f.[0],f.[1]))
  |> Observable.together obsTopo

let obsSituM = 
  Metrics.obsAll 
  |> Observable.choose (function Metrics.SitState s -> Some s | _ -> None)
  |> Observable.map (fun l -> l |> List.toSeq |> Seq.map (fun f -> f.[0],f.[1]))
  |> Observable.together obsSituational

let obsNormM = 
  Metrics.obsAll 
  |> Observable.choose (function Metrics.NormState s -> Some s | _ -> None)
  |> Observable.map (fun l -> l |> List.toSeq |> Seq.map (fun f -> f.[0],f.[1]))
  |> Observable.together obsNorm

let obsHistM = 
  Metrics.obsAll 
  |> Observable.choose (function Metrics.HistState s -> Some s | _ -> None)
  |> Observable.map (fun l -> printfn "Hist %d" l.Length; l |> List.toSeq |> Seq.map (fun f -> f.[0],f.[1]))
  |> Observable.together obsHist
