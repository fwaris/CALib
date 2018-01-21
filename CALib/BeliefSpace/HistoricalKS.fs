module HistoricalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 3.0
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
    (i1.Parms,i2.Parms) 
    ||> Array.exists2 (fun p1 p2 ->  p1 - p2 |> abs > th_significance)

type ChangeEvent<'k> = {Best:Individual<'k>; Direction:Dir array}

type HistoryState<'k> = 
    {
        Window      : int
        Distance    : float array
        Direction   : Dir array
        Events      : ChangeEvent<'k> list
    }

let log events = events |> List.map (fun {Best=b; Direction=_} -> b.Parms) |> Metrics.MetricMsg.HistState |> Metrics.postAll

let create (parmDefs:Parm[]) isBetter window =
    let create history fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Historical
            Accept      = fAccept fInfluence history
            Influence   = fInfluence history
        }

    let rec acceptance 
        fInfluence 
        ({Window=win; Events=events} as history)
        (voters:Individual<_> array) =
        match voters with
        | [||] -> voters,create history acceptance fInfluence
        | inds ->
            let rBest = inds.[0] //assume best individual is first 
            let nBest = 
                match events with
                | []                                                -> Some rBest
                | b::_ when isBetter rBest.Fitness b.Best.Fitness
                       && isSignificantlyDifferent rBest b.Best     -> Some rBest
                | _                                                 -> None
            match nBest with
            | None -> voters, create history acceptance fInfluence
            | Some nBest ->
                let pBest = match events with [] -> nBest | b::_ -> b.Best
                let eventDirection  = (pBest.Parms,nBest.Parms) ||> Array.mapi2 (dir parmDefs)
                let changeEvent     = {Best=nBest; Direction=eventDirection}
                let events          = changeEvent::events |> List.truncate win
                let earliestEvent = events.[events.Length - 1]
                let distance      = (nBest.Parms,earliestEvent.Best.Parms) ||> Array.map2 (fun p n -> abs ( p - n))
                let direction     = (nBest.Parms,earliestEvent.Best.Parms) ||> Array.mapi2 (dir parmDefs)
                let updatedHistory =
                    {
                        Window      = win
                        Distance    = distance
                        Direction   = direction
                        Events      = events
                    }

                #if _LOG_
                log events
                #endif

                voters, create updatedHistory acceptance fInfluence
    
    let influence {Events=events} s (ind:Individual<_>) =
        let ev = events.[rnd.Value.Next(0,events.Length)]
        if isBetter ev.Best.Fitness ind.Fitness then
            ev.Best |> influenceInd parmDefs s eSigma ind
        else
            evolveInd parmDefs s eSigma ind

    let initialHistory = {Window=window; Distance=[||]; Direction=[||]; Events=[]}
       
    create initialHistory acceptance influence


