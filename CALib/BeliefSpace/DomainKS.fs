module DomainKS
open CA
open CAUtils
open CAEvolve

type Slope = {Index:int; Magnitude:float; Direction:Dir}

let rateOfImprovement oldFitness newFitness isBetter epsilon =
    let denominator = parmToFloat epsilon
    if oldFitness = newFitness then 
        Flat,0.
    elif isBetter newFitness oldFitness then
        Up, (abs (newFitness-oldFitness)) / denominator
    else
        Down,(abs (newFitness-oldFitness)) / denominator

let maxSlope isBetter fitness oldFitness parms  =
    let parms    = Array.copy parms
    let epsilons = parms |> Array.map epsilon
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
        (newBestInds : Individual<_> array) =
        match newBestInds with
        | [||] -> newBestInds, create (prevExemplars,pBestSlope) acceptance fInfluence
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
                [|nBest|], create (exemplars,slope) acceptance fInfluence
            | None -> 
                [||], create (prevExemplars,pBestSlope) acceptance fInfluence

    let influence (exemplars,gBestSlope) s (ind:Individual<_>) =
        let (slope,parms) = maxSlope isBetter fitness ind.Fitness ind.Parms
        let maxParm = parms.[slope.Index]
        //printfn "maxParm: %A" maxParm
        let parm =
            match slope.Direction with
            | Up   -> slideUp s maxParm
            | Down -> slideDown s maxParm
            | Flat -> 
                match gBestSlope.Direction with
                | Up    -> slideUp s parms.[slope.Index]
                | Down  -> slideDown s parms.[slope.Index]
                | Flat  -> evolveS s (parms.[slope.Index])
        parms.[slope.Index] <- parm
        {ind with Parms=parms}
       
    create ([],{Index=0; Direction=Flat; Magnitude=0.}) acceptance influence
