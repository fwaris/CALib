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

let create parmDefs isBetter (fitness:Fitness) =
    let create fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Domain
            Accept      = fAccept
            Influence   = fInfluence
        }

    let influence influenceLevel (ind:Individual<_>) =
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

    let rec acceptance 
        envChanged
        (voters : Individual<_> array) = voters, create acceptance influence
       
    create acceptance influence
