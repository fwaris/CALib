module MetaLrn
open CA

(*
scheme performance: 
how quickly best was achieved
track the number of genations to best relative 
to total number of generations in regime (i.e. before environment change)

*)

let WARM_UP = 5

type Scheme<'s> = {SchId:int; Scheme:'s}

type Regime = {Best:float; GensToBest:int; RegimeGens:int; Scheme:int}

type MLState<'a> =
    {
        Schemes : Map<int,Scheme<'a>>
        Regimes : Regime list
        Sign    : float
    }

let currentScheme state = state.Schemes.[state.Regimes.Head.Scheme].Scheme

let initML cmprtr (schemes:'a seq) =
    let sMap = schemes |> Seq.mapi (fun i s-> i,{SchId=i;Scheme=s}) |> Map.ofSeq
    let sign = if cmprtr 1.0 0.0 then 1.0 else -1.0
    let extrema = if sign > 0.0 then System.Double.MinValue else System.Double.MaxValue
    {
        Schemes = sMap
        Regimes = [{Best=extrema; GensToBest=0; RegimeGens=0; Scheme=0}]
        Sign    = sign
    }

let updateRegime mlState (pop:Population<_>) =
    let sign = mlState.Sign
    let regime,rest = match mlState.Regimes with h::t -> h,t | _ -> failwith "empty regimes list"
    let gen = regime.RegimeGens + 1
    let cndtBst = pop |> Array.map (fun i->i.Fitness*sign) |> Array.max
    let foundBetter = cndtBst > regime.Best
    let regime =
        { regime with
            Best = if foundBetter then cndtBst else regime.Best
            GensToBest = if foundBetter then gen else regime.GensToBest
            RegimeGens = gen
        }
    { mlState with Regimes=regime::rest}

let roundRobbinRegime state = 
    let idx = (List.head state.Regimes).Scheme + 1 % state.Schemes.Count
    let scheme = state.Schemes.[idx]
    let extrema = if state.Sign > 0.0 then System.Double.MinValue else System.Double.MaxValue
    let rgm = {Best=extrema; GensToBest=0; RegimeGens=0; Scheme=idx}
    { state with
        Regimes = rgm::state.Regimes
    }

let perfBasedRegime state =
    let wtdSchms = 
        state.Regimes 
        |> List.map(fun r -> r.Scheme, float r.GensToBest / float r.RegimeGens) 
        |> List.groupBy fst
        |> List.map (fun (s,xs) -> s,xs |> List.averageBy snd)
        |> List.toArray
    let (_,wheel) = Probability.createWheel wtdSchms
    let idx = Probability.spinWheel wheel
    let extrema = if state.Sign > 0.0 then System.Double.MinValue else System.Double.MaxValue
    let rgm = {Best=extrema; GensToBest=0; RegimeGens=0; Scheme=idx}
    { state with
        Regimes = rgm::state.Regimes
    }

let regimeChanged state = 
    if state.Regimes.Length < WARM_UP
    then roundRobbinRegime state 
    else perfBasedRegime state
