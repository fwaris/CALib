module SituationalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 0.3

type State<'a> = {Exemplars:Individual<'a> list; SpinWheel:(int*float)array}

let fitweights isBetter (exemplars:Individual<_> list) =
    let tx m = if isBetter 2. 1. then m else 1.0 / m
    let wghts = exemplars |> List.map (fun i-> tx i.Fitness)
    wghts |> List.toArray

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
        (newlyAcceptedInds : Individual<_> array) =
        match newlyAcceptedInds with
        | [||] -> failwith "SituationalKS.acceptance : accepted individual list empty"
        | inds ->
            let rBest = inds.[0] //assume best individual is first for the latest genertion
            let nBest = 
                match state.Exemplars with
                | []                                                 -> Some rBest
                | pBest::_ when isBetter rBest.Fitness pBest.Fitness -> Some rBest
                | _                                                  -> None
            match nBest with
            | Some nBest ->
                let exemplars = nBest::state.Exemplars |> List.truncate maxExemplars
                let weights = fitweights isBetter exemplars
                let wheel = Probability.createWheel weights
                let state = {Exemplars=exemplars; SpinWheel=wheel}
                [|nBest|], create state acceptance fInfluence
            | None -> [||], create state acceptance fInfluence

    
    let influence state s (ind:Individual<_>) =
        match state.Exemplars with
        | [] -> ind
        | x -> 
            let i = Probability.spinWheel state.SpinWheel
            let choosen = x.[i]
            printfn "sit i = %d %A" i choosen
            {ind with Parms=choosen.Parms |> Array.map (evolveS s eSigma)}
       
    create {Exemplars=[];SpinWheel=[||]} acceptance influence
