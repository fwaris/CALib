///Domain knowledge source
module DomainKS
open CA
open CAUtils
open CAEvolve
open DiffEvolutionKS
let eSigma = 0.003  //exploratory index constant for Domain

///Slope information
type Slope = {Index:int; Magnitude:float; Direction:Dir}

let rateOfImprovement oldFitness newFitness isBetter denominator =
    if oldFitness = newFitness then 
        Flat,0.
    elif isBetter newFitness oldFitness then
        Up, (abs (newFitness-oldFitness)) / denominator
    else
        Down,(abs (newFitness-oldFitness)) / denominator

let slopes ibest isBetter fitness oldFit (parmDefs:Parm[]) parms =
    parms
    |> Array.mapi (fun i p -> 
        let pDef = parmDefs.[i]
        let e = denominatorM pDef
        let p' =  clampP (p + e) pDef        
        parms.[i] <- p'
        let newFit = fitness parms
        CAUtils.Incidental.update ibest isBetter newFit parms
        let partialSlope = rateOfImprovement oldFit newFit isBetter e
        parms.[i] <- p
        partialSlope)

let construct state fAccept fInfluence : KnowledgeSource<_> =
    {
        Type        = Domain
        Accept      = fAccept fInfluence state
        Influence   = fInfluence state
    }

///Domain default acceptance function
let rec defaultAcceptance fInfluence state envChanged voters = voters, construct state defaultAcceptance fInfluence

///Domain default influence function
let defaultInfluence state iBest _ influenceLevel (ind:Individual<_>) =
    //mutation
    let oldFit = ind.Fitness //state.Fitness.Value ind.Parms //cannot rely on existing fitness due to allowance for multiple KS influences therefore reevaluate
    let slopes = slopes iBest state.IsBetter state.Fitness.Value oldFit state.ParmDefs ind.Parms
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

///Create Domain knowledge source
let create parmDefs optKind (fitness:Fitness) =

    let state = DiffEvolutionKS.initState parmDefs (CAUtils.comparator optKind) fitness

    construct state defaultAcceptance defaultInfluence
