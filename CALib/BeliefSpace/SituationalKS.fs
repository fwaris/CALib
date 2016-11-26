module SituationalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 3.

let create isBetter maxExemplars =
    let create (examplars:Individual<_> list) fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Situational
            Accept      = fAccept fInfluence examplars
            Influence   = fInfluence examplars
        }

    let rec acceptance 
        fInfluence 
        (prevExemplars : Individual<_> list) 
        (newlyAcceptedInds : Individual<_> array) =
        match newlyAcceptedInds with
        | [||] -> failwith "SituationalKS.acceptance : accepted individual list empty"
        | inds ->
            let rBest = inds.[0] //assume best individual is first for the latest genertion
            let nBest = 
                match prevExemplars with
                | []                                                 -> Some rBest
                | pBest::_ when isBetter rBest.Fitness pBest.Fitness -> Some rBest
                | _                                                  -> None
            match nBest with
            | Some nBest ->
                let exemplars = nBest::prevExemplars |> List.truncate maxExemplars
                [|nBest|], create exemplars acceptance fInfluence
            | None -> [||], create prevExemplars acceptance fInfluence

    
    let influence exemplars s (ind:Individual<_>) =
        match exemplars with
        | [] -> ind
        | best::_ -> best |> influenceInd s eSigma ind
       
    create [] acceptance influence
