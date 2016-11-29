module DomainKS2
open CA
open CAUtils
open CAEvolve

let eSigma = 0.6

type Slope = {Index:int; Magnitude:float; Direction:Dir}

let rateOfImprovement oldFitness newFitness isBetter epsilon =
    let denominator = parmToFloat epsilon
    if oldFitness = newFitness then 
        Flat,0.
    elif isBetter newFitness oldFitness then
        Up, (abs (newFitness-oldFitness)) / denominator
    else
        Down,(abs (newFitness-oldFitness)) / denominator

let slopes isBetter fitness oldFit parms =
    let parms = Array.copy parms
    parms
    |> Array.mapi (fun i p -> 
        let e = epsilonM p
        let p' =  parmAdd p e
        parms.[i] <- p'
        let newFit = fitness parms
        let partialSlope = rateOfImprovement oldFit newFit isBetter e
        parms.[i] <- p
        partialSlope)

let maxSlope isBetter fitness oldFitness parms  =
    let parms    = Array.copy parms
    let epsilons = parms |> Array.map epsilonM
    let mutable maxS = Flat,0.
    let mutable maxI = 0
    for i in 0..parms.Length - 1 do
        let p = parms.[i]     //x
        let e = epsilons.[i]  //dx
        let p' = parmAdd p e
        parms.[i] <- p' //x + dx
        let newFitness = fitness parms
        parms.[i] <- p  //reset x
        let imp = rateOfImprovement oldFitness newFitness isBetter e
        match maxS,imp with
        | _    , (Flat,_)           -> ()
        | (_,a), (dir,b) when b > a -> maxS <- dir,b; maxI <- i
        | _                         -> ()
    {Index=maxI; Magnitude=snd maxS; Direction=fst maxS},parms    

let create isBetter fitness maxExemplars =
    let create state fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Domain
            Accept      = fAccept fInfluence state
            Influence   = fInfluence state
        }

    let rec acceptance 
        fInfluence 
        (prevExemplars : Individual<_> list, pBestSlope) 
        (voters : Individual<_> array) =
        match voters with
        | [||] -> voters, create (prevExemplars,pBestSlope) acceptance fInfluence
        | inds ->
            let rBest = inds.[0] //assume best individual is first
            let nBest =
                match prevExemplars with
                | []                                                    -> Some rBest
                | pBest::_ when isBetter rBest.Fitness pBest.Fitness    -> Some rBest
                | _                                                     -> None
            match nBest with
            | Some nBest ->
                let slope,_ = maxSlope isBetter fitness nBest.Fitness nBest.Parms
                let exemplars = nBest::prevExemplars |> List.truncate maxExemplars
                voters, create (exemplars,slope) acceptance fInfluence
            | None -> 
                voters, create (prevExemplars,pBestSlope) acceptance fInfluence

    let influence (exemplars,gBestSlope) s (ind:Individual<_>) =
        let slopes = slopes isBetter fitness ind.Fitness ind.Parms
        let parms =
            ind.Parms
            |> Array.mapi (fun i p ->
                let (dir,mag) = slopes.[i]
                match dir with
                | Up   -> slideUp s eSigma p
                | Down -> slideDown s eSigma p
                | Flat -> evolveS s eSigma p
            )
        {ind with Parms=parms}
       
    create ([],{Index=0; Direction=Flat; Magnitude=0.}) acceptance influence
