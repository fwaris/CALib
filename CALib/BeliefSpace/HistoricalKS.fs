module HistoricalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 0.3

//determine direction of change
let dir newParm prevParm = 
    match prevParm,newParm with
    | F(prevV,_,_),F(newV,_,_)  when newV > prevV       -> Up
    | F(prevV,_,_),F(newV,_,_)  when newV < prevV       -> Down
    | F(prevV,_,_),F(newV,_,_)                          -> Flat
    | F32(prevV,_,_),F32(newV,_,_)  when newV > prevV   -> Up
    | F32(prevV,_,_),F32(newV,_,_)  when newV < prevV   -> Down
    | F32(prevV,_,_),F32(newV,_,_)                      -> Flat
    | I(prevV,_,_),I(newV,_,_)  when newV > prevV       -> Up
    | I(prevV,_,_),I(newV,_,_)  when newV < prevV       -> Down
    | I(prevV,_,_),I(newV,_,_)                          -> Flat
    | I64(prevV,_,_),I64(newV,_,_)  when newV > prevV   -> Up
    | I64(prevV,_,_),I64(newV,_,_)  when newV < prevV   -> Down
    | I64(prevV,_,_),I64(newV,_,_)                      -> Flat
    | a,b -> failwithf "HistoricalKS: invalid combination of types for dir %A,%A" a b

let parmAvg count = function
    | F(v,_,_)      -> float v / float count
    | F32(v,_,_)    -> float v / float count
    | I(v,_,_)      -> float v / float count
    | I64(v,_,_)    -> float v / float count

let isSignificantlyDifferent i1 i2 =
    (i1.Parms,i2.Parms) 
    ||> Array.exists2 (fun p1 p2 -> parmToFloat p1 - parmToFloat p2 |> abs > 0.001)

type ChangeEvent<'k> = {Best:Individual<'k>; Direction:Dir array}
type History<'k> = 
    {
        Window      : int
        Distance    : Parm array
        Direction   : Dir array
        Events      : ChangeEvent<'k> list
    }

let create isBetter window =
    let create history fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Historical
            Accept      = fAccept fInfluence history
            Influence   = fInfluence history
        }

    let rec acceptance 
        fInfluence 
        ({Window=win; Events=events} as history)
        (inds:Individual<_> array) =
        match inds with
        | [||] -> [||],create history acceptance fInfluence
        | inds ->
            let rBest = inds.[0] //assume best individual is first 
            let nBest = 
                match events with
                | []                                                -> Some rBest
                | b::_ when isBetter rBest.Fitness b.Best.Fitness
                       && isSignificantlyDifferent rBest b.Best     -> Some rBest
                | _                                                 -> None
            match nBest with
            | None -> [||], create history acceptance fInfluence
            | Some nBest ->
                let pBest = match events with [] -> nBest | b::_ -> b.Best
                let eventDirection  = (pBest.Parms,nBest.Parms) ||> Array.map2 dir
                let changeEvent     = {Best=nBest; Direction=eventDirection}
                let events          = changeEvent::events |> List.truncate win
                let earliestEvent = events.[events.Length - 1]
                let distance      = (nBest.Parms,earliestEvent.Best.Parms) ||> Array.map2 parmDiff
                let direction     = (nBest.Parms,earliestEvent.Best.Parms) ||> Array.map2 dir
                let updatedHistory =
                    {
                        Window      = win
                        Distance    = distance
                        Direction   = direction
                        Events      = events
                    }
                [|nBest|], create updatedHistory acceptance fInfluence
    
    let influence {Events=events} s (ind:Individual<_>) =
        let ev = events.[rnd.Value.Next(0,events.Length-1)]
        if isBetter ev.Best.Fitness ind.Fitness then
            ev.Best |> influenceInd s eSigma ind
        else
            evolveInd s eSigma ind

    let initialHistory = {Window=window; Distance=[||]; Direction=[||]; Events=[]}
       
    create initialHistory acceptance influence


