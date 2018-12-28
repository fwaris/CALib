module MetaLrn2
open CA

(*
Meta learning based on multi-arm bandit approach
scheme performance: 
how quickly best was achieved
track the number of genations to best relative 
to total number of generations in regime (i.e. before environment change)

*)

let MAX_REGIMES = 100

type Regime = {Best:float; GensToBest:int; RegimeGens:int; Scheme:float}

type MLState =
    {
        BaseLevel   : float
        Range       : float*float
        Regimes     : Regime list
        Sign        : float
    }

let currentScheme state = state.Regimes.Head.Scheme

let addRegime level state =
    let extrema = if state.Sign > 0.0 then System.Double.MinValue else System.Double.MaxValue
    { state with
        Regimes = {Best=extrema; GensToBest=0; RegimeGens=0; Scheme=level}::state.Regimes |> List.truncate MAX_REGIMES
    }

let initML cmprtr range (baseScheme:float) =
    let sign = if cmprtr 1.0 0.0 then 1.0 else -1.0
    {
        BaseLevel   = baseScheme
        Range       = range
        Regimes     = []
        Sign        = sign
    }
    |> addRegime baseScheme

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

let perf {GensToBest=b; RegimeGens=r} = 1.0 - (float b/float r)

let regimeChanged state =
    let newLevel = 
        match state.Regimes with
        | [] -> failwith "empty regimes not expected"
        | [x] -> Probability.RNG.Value.Next(fst state.Range,snd state.Range)
        | a::b::_ ->
            match perf a, perf b, a.Scheme, b.Scheme with
            | pa,pb,a,b when  pa > pb && a > b -> Probability.RNG.Value.Next(a,snd state.Range)
            | pa,pb,a,b when  pa > pb && a < b -> Probability.RNG.Value.Next(fst state.Range, a)
            | pa,pb,a,b when  pa < pb && a > b -> Probability.RNG.Value.Next(fst state.Range,b)
            | pa,pb,a,b when  pa < pb && a < b -> Probability.RNG.Value.Next(b,snd state.Range)
            | _                                ->  Probability.RNG.Value.Next(fst state.Range,snd state.Range)
    addRegime newLevel state

