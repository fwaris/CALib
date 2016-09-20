module SituationalKS
open CA
open CAUtils

let create isBetter maxExemplars =
    let create (examplars:Individual list) fAccept fInfluence : KnowledgeSource =
        {
            Type        = Situational
            Accept      = fAccept fInfluence examplars
            Influence   = fInfluence examplars
        }

    let rec acceptance 
        fInfluence 
        (prevExemplars : Individual list) 
        (newlyAcceptedInds : Individual array) =
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

    
    let influence exemplars (ind:Individual) =
        match exemplars with
        | [] -> ind
        | best::_ -> best |> influenceInd  ind
       
    create [] acceptance influence
