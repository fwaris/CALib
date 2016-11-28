module SituationalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 3.0

type State<'a> = 
    {
        Exemplars    :   Individual<'a> array
        SpinWheel    :   (int*float)array
    }

let fitweights isBetter (exemplars:Individual<_> array) =
    let tx m = if isBetter 2. 1. then m else 1.0 / m
    let wghts = exemplars |> Array.map (fun i-> tx i.Fitness)
    wghts 

let parmDiversity p1 p2 = 
    (p1,p2)
    ||> Array.map2 (fun a b -> parmDiff a b |> parmToFloat |> abs) 
    |> Array.sum

let pickExamplars isBetter prevE voters =
    let tx = if isBetter 2. 1. then -1. else 1.
    let ex = Seq.append prevE  voters |> Seq.toArray
    let ex = Array.sortBy (fun (x:Individual<_>) -> tx * x.Fitness) ex
    let best = ex.[0]
    let dx =      //ordered by increasing diversity from best
        ex.[1..] 
        |> Array.map (fun indv -> indv, parmDiversity indv.Parms best.Parms) 
        |> Array.sortBy (fun (i,x) -> -x)
    Array.append [|best|] (dx |> Array.map fst)

let create isBetter maxExemplars =
    let create state fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Situational
            Accept      = fAccept fInfluence state
            Influence   = fInfluence state
        }

    let rec acceptance 
        fInfluence 
        state
        (voters : Individual<_> array) =
        let explrs = pickExamplars isBetter state.Exemplars voters |> Array.truncate maxExemplars
        let weights = fitweights isBetter explrs
        let wheel = Probability.createWheel weights
        let state = {Exemplars=explrs; SpinWheel=wheel}
        voters, create state acceptance fInfluence
    
    let influence state s (ind:Individual<_>) =
        match state.Exemplars with
        | [||] -> ind
        | x -> 
            let i = Probability.spinWheel state.SpinWheel
            let choosen = x.[i]
            printfn "sit i = %d %A" i choosen
            {ind with Parms=choosen.Parms |> Array.map (evolveS s eSigma)}      
    create {Exemplars=[||];SpinWheel=[||]} acceptance influence
