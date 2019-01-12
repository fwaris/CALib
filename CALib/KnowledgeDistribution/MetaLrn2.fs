[<RequireQualifiedAccess>]
module MetaLrn2
open CA

(*
Meta learning based on multi-arm bandit approach and thompson sampling
scheme performance: 
how quickly best was achieved
track the number of genations to best relative 
to total number of generations in regime (i.e. before environment change)

*)

let MAX_REGIMES = 100

type Scheme<'s> = {SchId:int; Scheme:'s}

type Regime = {Best:float; ImprovementCount:int; GensToBest:int; RegimeGens:int; Scheme:int}

let newRegime sign = 
    let extremum = if sign > 0.0 then System.Double.MinValue else System.Double.MaxValue
    {Best=extremum; ImprovementCount=0; GensToBest=0; RegimeGens=0; Scheme=0}

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
    let extremum = if sign > 0.0 then System.Double.MinValue else System.Double.MaxValue
    {
        Schemes = sMap
        Regimes = [newRegime sign]
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
            ImprovementCount = if foundBetter then regime.ImprovementCount + 1 else regime.ImprovementCount
            RegimeGens = gen
        }
    { mlState with Regimes=regime::rest}

let roundRobbinRegime state = 
    let idx = 
        match state.Regimes with 
        | [] -> 0
        | x::_ -> x.Scheme + 1 % state.Schemes.Count
    let extremum = if state.Sign > 0.0 then System.Double.MinValue else System.Double.MaxValue
    let rgm = {Best=extremum; GensToBest=0; ImprovementCount = 0; RegimeGens=0; Scheme=idx}
    { state with
        Regimes = rgm::state.Regimes
    }

let gaussianPosterior (rewards: (int*float) list) = 
    let mu = rewards |> List.averageBy snd
    let sgma  = 1.0 / float (rewards.Length + 1)
    mu,sgma

let perfBasedRegime state =
    let wtdSchms = 
        state.Regimes 
        |> List.filter (fun r->r.RegimeGens > 0)
        //|> List.map (fun r -> r.Scheme, abs(1.0 - ( float r.GensToBest / float r.RegimeGens / float r.ImprovementCount))) //map reward for each try
        //|> List.map (fun r -> r.Scheme, state.Sign * r.Best)// abs(1.0 - ( float r.GensToBest / float r.RegimeGens / float r.ImprovementCount))) //map reward for each try
        |> List.map (fun r -> r.Scheme, state.Sign * r.Best * abs(1.0 - ( float r.GensToBest / float r.RegimeGens))) //map reward for each try
        |> List.groupBy fst
        |> List.map (fun (s,xs) -> s,gaussianPosterior xs)
        |> List.toArray
    if wtdSchms.Length = 0 then
        printfn "wtdSchms empty"
        roundRobbinRegime state
    else
        let samples = wtdSchms |> Array.map(fun (s,(mu,sgma)) -> s, Probability.GAUSS mu sgma) //thompson sampling with gaussian priors
        let arm,_ = samples |> Array.maxBy snd
        let extremum = if state.Sign > 0.0 then System.Double.MinValue else System.Double.MaxValue
        let rgm = {newRegime state.Sign with Scheme=arm}
        { state with
            Regimes = rgm::state.Regimes
        }

let regimeChanged state = 
    let state = 
        if state.Regimes.Length < state.Schemes.Count //sample each scheme at least once
        then roundRobbinRegime state 
        else perfBasedRegime state
    { state with
        Regimes = state.Regimes |> List.truncate MAX_REGIMES
    }
