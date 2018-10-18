module DomainKS2
open CA
open CAUtils
open CAEvolve
open HistoricalKS

let eSigma = 0.003

type Slope = {Index:int; Magnitude:float; Direction:Dir}

type DomainState = {ParmDefs:Parm[]; IsBetter:Comparator; Fitness:Fitness}

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
        let e = denominatorM pDef
        let p' =  p + e
        parms.[i] <- p'
        let newFit = fitness parms
        let partialSlope = rateOfImprovement oldFit newFit isBetter e
        parms.[i] <- p
        partialSlope)


let initialState parmDefs isBetter fitness = {ParmDefs=parmDefs; IsBetter=isBetter; Fitness=fitness}

let construct state fAccept fInfluence : KnowledgeSource<_> =
    {
        Type        = Domain
        Accept      = fAccept fInfluence state
        Influence   = fInfluence state
    }

let rec defaultAcceptance fInfluence state envChanged voters = voters, construct state defaultAcceptance fInfluence

let defaultInfluence state influenceLevel (ind:Individual<_>) =
    //mutation
    let oldFit = state.Fitness.Value ind.Parms //cannot rely on existing fitness due to multiple KS influences therefore reevaluate
    //let slopes = slopes isBetter fitness.Value ind.Fitness parmDefs ind.Parms
    let slopes = slopes state.IsBetter state.Fitness.Value oldFit state.ParmDefs ind.Parms
    let parms = ind.Parms
    let z = zsample() |> abs
    let stepSize = z * influenceLevel * eSigma
    ind.Parms |> Array.iteri(fun i p ->
        parms.[i] <-
            let (dir,slope) = slopes.[i]
            match dir with
            | Up   -> stepUp stepSize slope state.ParmDefs.[i] p
            | Down -> stepDown stepSize slope state.ParmDefs.[i] p 
            | Flat -> p)//evolveS s eSigma p)
    ind

let create parmDefs isBetter (fitness:Fitness) =

    let state = initialState parmDefs isBetter fitness

    construct state defaultAcceptance defaultInfluence
