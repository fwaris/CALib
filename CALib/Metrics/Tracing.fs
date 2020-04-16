///diagnostic code for 'peering' into a CA run
module Tracing

let cts = new System.Threading.CancellationTokenSource()

type O1s = System.IObservable<(float*float)seq>
type F1s = (float*float) [] -> unit
type O2s =  System.IObservable<(string*float) seq>
type F2s = (string*float) list -> unit
type O3 = System.IObservable<float>
type F3 = float->unit
type O4 = System.IObservable<string option>
type F4 = string option -> unit
type O5 = System.IObservable<int*float>
type F5 = (int*float)->unit

type ObsBucket =
    {
        obsAll : O1s
        fpAll : F1s

        obsKSCounts : O2s
        fpKSCounts : F2s

        obsDomain : O1s
        fpDomain : F1s

        obsSecDmn : O1s
        fpSecDmn : F1s

        obsSituational : O1s
        fpSituational : F1s

        obsNorm : O1s
        fpNorm : F1s

        obsHist : O1s
        fpHist : F1s

        obsTopo : O1s
        fpTopo : F1s

        obsDispersion : O5
        fpDispersion : F5

        obsSeg : O5
        fpSeg : F3

        obsDfsn : O5
        fpDfsn : F3

        obsDist : O5
        fpDist : F3

        obsMax : O3
        fpMax : F3

        obsCurrBest : O1s
        fpCurrBest  : F1s

        obsMaxCone : O3
        fpMaxCone : F3

        obsBkground : O4
        fpBkground : F4

    }


let createObservables id = 
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
    let obsDfsn_,fpDfsn= Observable.createObservableAgent<float> cts.Token
    let obsDfsn = obsDfsn_ |> Observable.withI
    let obsDist_,fpDist = Observable.createObservableAgent<float>(cts.Token)
    let obsDist = obsDist_ |> Observable.withI
    let obsMax,fpMax = Observable.createObservableAgent<float>(cts.Token) 
    let obsMaxCone,fpMaxCone= Observable.createObservableAgent<float>(cts.Token)
    let obsBkground, fpBkground = Observable.createObservableAgent<string option>(cts.Token)
    let obsCurrBest,fpCurrBest = Observable.createObservableAgent<(float*float) seq> cts.Token
    {
        obsAll = obsAll
        fpAll = fpAll

        obsKSCounts = obsKSCounts
        fpKSCounts = fpKSCounts

        obsDomain = obsDomain
        fpDomain = fpDomain

        obsSecDmn = obsSecDmn
        fpSecDmn = fpSecDmn

        obsSituational = obsSituational
        fpSituational = fpSituational

        obsNorm = obsNorm
        fpNorm = fpNorm

        obsHist = obsHist
        fpHist = fpHist

        obsTopo = obsTopo
        fpTopo = fpTopo

        obsDispersion = obsDispersion
        fpDispersion = fpDispersion

        obsSeg = obsSeg
        fpSeg = fpSeg

        obsDfsn = obsDfsn
        fpDfsn = fpDfsn

        obsDist = obsDist
        fpDist = fpDist

        obsMax = obsMax
        fpMax = fpMax

        obsCurrBest = obsCurrBest
        fpCurrBest = fpCurrBest

        obsMaxCone = obsMaxCone
        fpMaxCone = fpMaxCone

        obsBkground = obsBkground
        fpBkground = fpBkground

    }

////KS-specific internal states
//let obsTopoM = 
//  Metrics.obsAll 
//  |> Observable.choose (function Metrics.TopoState s -> Some s | _ -> None)
//  |> Observable.map (fun l ->  l |> List.toSeq |> Seq.map (fun f -> f.[0],f.[1]))
//  |> Observable.together obsTopo

//let obsSituM = 
//  Metrics.obsAll 
//  |> Observable.choose (function Metrics.SitState s -> Some s | _ -> None)
//  |> Observable.map (fun l -> l |> List.toSeq |> Seq.map (fun f -> f.[0],f.[1]))
//  |> Observable.together obsSituational

//let obsNormM = 
//  Metrics.obsAll 
//  |> Observable.choose (function Metrics.NormState s -> Some s | _ -> None)
//  |> Observable.map (fun l -> l |> List.toSeq |> Seq.map (fun f -> f.[0],f.[1]))
//  |> Observable.together obsNorm

//let obsHistM = 
//  Metrics.obsAll 
//  |> Observable.choose (function Metrics.HistState s -> Some s | _ -> None)
//  |> Observable.map (fun l -> printfn "Hist %d" l.Length; l |> List.toSeq |> Seq.map (fun f -> f.[0],f.[1]))
//  |> Observable.together obsHist
