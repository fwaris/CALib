module DomainKS2
open CA
open CAUtils
open CAEvolve

let eSigma = 0.003

type Slope = {Index:int; Magnitude:float; Direction:Dir}

let rateOfImprovement oldFitness newFitness isBetter denominator =
    if oldFitness = newFitness then 
        Flat,0.
    elif isBetter newFitness oldFitness then
        Up, (abs (newFitness-oldFitness)) / denominator
    else
        Down,(abs (newFitness-oldFitness)) / denominator

let slopes isBetter fitness oldFit (parmDefs:Parm[]) parms =
    parms
    |> Array.mapi (fun i p -> 
        let pDef = parmDefs.[i]
        let e = epsilonM pDef
        let p' =  p + e
        parms.[i] <- p'
        let newFit = fitness parms
        let partialSlope = rateOfImprovement oldFit newFit isBetter e
        parms.[i] <- p
        partialSlope)

let create parmDefs isBetter (fitness:Fitness) maxExemplars =
    let create state fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Domain
            Accept      = fAccept fInfluence state
            Influence   = fInfluence state
        }

    let rec acceptance 
        fInfluence 
        (prevExemplars : Individual<_> list) 
        (voters : Individual<_> array) =
        match voters with
        | [||] -> voters, create prevExemplars acceptance fInfluence
        | inds ->
            let rBest = inds.[0] //assume best individual is first
            let nBest =
                match prevExemplars with
                | []                                                    -> Some rBest
                | pBest::_ when isBetter rBest.Fitness pBest.Fitness    -> Some rBest
                | _                                                     -> None
            match nBest with
            | Some nBest ->
                let exemplars = nBest::prevExemplars |> List.truncate maxExemplars
                voters, create exemplars acceptance fInfluence
            | None -> 
                voters, create prevExemplars acceptance fInfluence

    let influence exemplars influenceLevel (ind:Individual<_>) =
        //mutation
        let slopes = slopes isBetter fitness.Value ind.Fitness parmDefs ind.Parms
        let parms = ind.Parms
        ind.Parms |> Array.iteri(fun i p ->
            parms.[i] <-
                let (dir,mag) = slopes.[i]
                match dir with
                | Up   -> slideUp (influenceLevel*mag) eSigma parmDefs.[i] p
                | Down -> slideDown (influenceLevel*mag) eSigma parmDefs.[i] p
                | Flat -> p)//evolveS s eSigma p)
        ind
       
    create [] acceptance influence
