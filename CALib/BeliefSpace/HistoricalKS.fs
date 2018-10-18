﻿module HistoricalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 0.5
let th_significance = 0.001

//determine direction of change
let dir (parmDefs:Parm[]) i prevVal newVal  = 
    match parmDefs.[i],prevVal,newVal with 
    | F(_,_,_),pV,iV when pV > iV   -> Up 
    | F(_,_,_),pV,iV when pV < iV   -> Down 

    | I(_,mn,mx),pV,iV when pV > iV -> Up
    | I(_,mn,mx),pV,iV when pV < iV -> Down
    | _                             -> Flat

let parmAvg count = function
    | F(v,_,_)      -> float v / float count
    | I(v,_,_)      -> float v / float count

let isSignificantlyDifferent i1 i2 =
    (i1,i2) 
    ||> Array.exists2 (fun p1 p2 ->  p1 - p2 |> abs > th_significance)

type HistoryState = 
    {
        Window      : int
        Events      : Marker list
        IsBetter    : Comparator
        ParmDefs    : Parm[]
    }

let log events = events |> List.map (fun {MParms=p} -> p) |> Metrics.MetricMsg.HistState |> Metrics.postAll

let construct state fAccept fInfluence : KnowledgeSource<_> =
    {
        Type        = Historical
        Accept      = fAccept fInfluence state
        Influence   = fInfluence state
    }

let defaultInfluence state s (ind:Individual<_>) =
    //printf "H:%d %0.2f %A->" ind.Id ind.Fitness ind.Parms
    //let iBefore = ind.Parms |> Array.copy
    let ev = state.Events.[rnd.Value.Next(0,state.Events.Length)]
    let ind' = 
        if state.IsBetter ev.MFitness ind.Fitness then
            ev.MParms |> influenceInd state.ParmDefs s eSigma ind
        else
            evolveInd CAEvolve.RANGE_SCALER state.ParmDefs s eSigma ind

    //printfn "[%d] %A (%A)" ind'.Id ind'.Parms ev.MParms
    //let iAfter = ind'.Parms |> Array.copy
    ind'

let rec defaultAcceptance 
    fInfluence 
    state
    envChanged
    (voters:Individual<_> array) =
    let {Window=win;Events=events} = if envChanged then {state with Events=[]} else state
    match voters with
    | [||] -> voters,construct state defaultAcceptance fInfluence
    | inds ->
        let rBest = inds.[0] //assume best individual is first 
        let nBest = 
            match events with
            | []                                                -> Some rBest
            | b::_ when state.IsBetter rBest.Fitness b.MFitness
                    && isSignificantlyDifferent rBest.Parms b.MParms     -> Some rBest
            | _                                                 -> None
        match nBest with
        | None -> voters, construct state defaultAcceptance fInfluence
        | Some nBest ->
            let pBest = match events with [] -> nBest.Parms | b::_ -> b.MParms
            //let eventDirection = (pBest,nBest.Parms) ||> Array.mapi2 (dir parmDefs)
            let changeEvent    = toMarker nBest
            let events         = changeEvent::events |> List.truncate win
            //let earliestEvent = events.[events.Length - 1]
            //let distance      = (nBest.Parms,earliestEvent.MParms) ||> Array.map2 (fun p n -> abs ( p - n))
            //let direction     = (nBest.Parms,earliestEvent.MParms) ||> Array.mapi2 (dir parmDefs)
            let updatedHistory =
                {state with
                    Window      = win
                    Events      = events
                }

            #if _LOG_
            log events
            #endif

            voters, construct updatedHistory defaultAcceptance fInfluence


let initialState (parmDefs:Parm[]) isBetter window = {Window=window; Events=[]; IsBetter=isBetter; ParmDefs=parmDefs}

let create (parmDefs:Parm[]) isBetter window =

    let state = initialState parmDefs isBetter window
       
    construct state defaultAcceptance defaultInfluence


