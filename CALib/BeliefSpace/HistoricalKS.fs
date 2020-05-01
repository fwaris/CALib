/// History knowledge source
module HistoricalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 0.5 //exploratory index constant for history

let th_significance = 0.001 //significant difference threshold

///Determine direction of change of the ith parameter given prev and new values
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

//State kept by History
type HistoryState = 
    {
        Window      : int
        Events      : Marker list
        IsBetter    : Comparator
        ParmDefs    : Parm[]
    }

#if CA_RUNTIME 
#else
let log events = events |> List.map (fun {MParms=p} -> p) |> Metrics.MetricMsg.HistState |> Metrics.postAll
#endif

let construct state fAccept fInfluence : KnowledgeSource<_> =
    {
        Type        = Historical
        Accept      = fAccept fInfluence state
        Influence   = fInfluence state
    }

///History default influence function
let defaultInfluence state _ _ s (ind:Individual<_>) =
    let ev = state.Events.[rnd.Value.Next(0,state.Events.Length)]
    let ind' = 
        if state.IsBetter ev.MFitness ind.Fitness then
            ev.MParms |> influenceInd state.ParmDefs s eSigma ind
        else
            evolveInd CAEvolve.RANGE_SCALER state.ParmDefs s eSigma ind
    ind'

///History default acceptance function
let rec defaultAcceptance 
    fInfluence 
    state
    envChanged
    (voters:Individual<_> array) =
 
    let {Window=win;Events=events} = 
        match envChanged with
        | Adjust    -> {state with Events=[]}
        | _         -> state

    match voters with
    | [||] -> voters,construct state defaultAcceptance fInfluence
    | inds ->
        let rBest = inds.[0] //assume best individual is first 

        let nBest = 
            match events with
            | []                                                     -> Some rBest
            | b::_ when state.IsBetter rBest.Fitness b.MFitness
                    && isSignificantlyDifferent rBest.Parms b.MParms -> Some rBest
            | _                                                      -> None

        match nBest with
        | None -> voters, construct state defaultAcceptance fInfluence
        | Some nBest ->
            let pBest = match events with [] -> nBest.Parms | b::_ -> b.MParms
            let changeEvent    = toMarker nBest
            let events         = changeEvent::events |> List.truncate win
            let updatedHistory =
                {state with
                    Window      = win
                    Events      = events
                }

            #if _LOG_
            log events
            #endif

            voters, construct updatedHistory defaultAcceptance fInfluence


let initState (parmDefs:Parm[]) isBetter window = 
    {
        Window=window
        Events=[]
        IsBetter=isBetter
        ParmDefs=parmDefs
    }

///Create History KS
let create (parmDefs:Parm[]) optKind window =

    let state = initState parmDefs (CAUtils.comparator optKind) window
       
    construct state defaultAcceptance defaultInfluence


