module HistoricalKS
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

type HistoryState<'k> = 
    {
        Window      : int
        Events      : Marker list
    }

let log events = events |> List.map (fun {MParms=p} -> p) |> Metrics.MetricMsg.HistState |> Metrics.postAll

let create (parmDefs:Parm[]) isBetter window =
    let create history fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Historical
            Accept      = fAccept fInfluence history
            Influence   = fInfluence history
        }

    let rec acceptance 
        fInfluence 
        history
        envChanged
        (voters:Individual<_> array) =
        let {Window=win;Events=events} = if envChanged then {Window=window;Events=[]} else history
        match voters with
        | [||] -> voters,create history acceptance fInfluence
        | inds ->
            let rBest = inds.[0] //assume best individual is first 
            let nBest = 
                match events with
                | []                                                -> Some rBest
                | b::_ when isBetter rBest.Fitness b.MFitness
                       && isSignificantlyDifferent rBest.Parms b.MParms     -> Some rBest
                | _                                                 -> None
            match nBest with
            | None -> voters, create history acceptance fInfluence
            | Some nBest ->
                let pBest = match events with [] -> nBest.Parms | b::_ -> b.MParms
                //let eventDirection = (pBest,nBest.Parms) ||> Array.mapi2 (dir parmDefs)
                let changeEvent    = toMarker nBest
                let events         = changeEvent::events |> List.truncate win
                //let earliestEvent = events.[events.Length - 1]
                //let distance      = (nBest.Parms,earliestEvent.MParms) ||> Array.map2 (fun p n -> abs ( p - n))
                //let direction     = (nBest.Parms,earliestEvent.MParms) ||> Array.mapi2 (dir parmDefs)
                let updatedHistory =
                    {
                        Window      = win
                        Events      = events
                    }

                #if _LOG_
                log events
                #endif

                voters, create updatedHistory acceptance fInfluence
    
    let influence {Events=events} s (ind:Individual<_>) =
        //printf "H:%d %0.2f %A->" ind.Id ind.Fitness ind.Parms
        //let iBefore = ind.Parms |> Array.copy
        let ev = events.[rnd.Value.Next(0,events.Length)]
        let ind' = 
          if isBetter ev.MFitness ind.Fitness then
              ev.MParms |> influenceInd parmDefs s eSigma ind
          else
              evolveInd parmDefs s eSigma ind

        //printfn "[%d] %A (%A)" ind'.Id ind'.Parms ev.MParms
        //let iAfter = ind'.Parms |> Array.copy
        ind'

    let initialHistory = {Window=window; Events=[]}
       
    create initialHistory acceptance influence


