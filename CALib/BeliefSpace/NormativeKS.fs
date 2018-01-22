module NormativeKS
open CA
open CAUtils
open CAEvolve

let eSigma = 2.0

type Norm = 
    {
        FitnessLo : float
        ParmLo    : float
        FitnessHi : float
        ParmHi    : float
    }

let updateNorms isBetter norms highPerfInd =
    norms 
    |> Array.mapi(fun i ({FitnessLo=fLo; ParmLo=pLo; FitnessHi=fHi; ParmHi=pHi} as norm) ->
        let parm = highPerfInd.Parms.[i]
        match 
            isBetter highPerfInd.Fitness fLo,      //is better than low fit
            isBetter highPerfInd.Fitness fHi with  //is better than high fit
        | true,true ->
            {
                FitnessLo  = highPerfInd.Fitness
                ParmLo     = parm
                FitnessHi  = highPerfInd.Fitness
                ParmHi     = parm
            }
        | true,false ->
            let isPrevHigher = pHi > parm //isHigher (pHi,parm)
            {
                FitnessLo  = highPerfInd.Fitness
                ParmLo     = parm
                FitnessHi  = if isPrevHigher then fHi else highPerfInd.Fitness
                ParmHi     = if isPrevHigher then pHi else parm
            }
        | false,true ->
            let isPrevLower = pLo < parm //isLower (pLo,parm)
            {
                FitnessLo  = if isPrevLower then fLo else highPerfInd.Fitness
                ParmLo     = if isPrevLower then pLo else parm
                FitnessHi  = highPerfInd.Fitness
                ParmHi     = parm
            }
        | false,false ->
            let isPrevHigher = pHi > parm // isHigher (pHi,parm)
            let isPrevLower  = pLo < parm // isLower (pLo,parm)
            {
                FitnessLo  = if isPrevLower then fLo else highPerfInd.Fitness
                ParmLo     = if isPrevLower then pLo else parm
                FitnessHi  = if isPrevHigher then fHi else highPerfInd.Fitness
                ParmHi     = if isPrevHigher then pHi else parm
            }
    )

let minP = function F(_,mn,_) -> mn | I(_,mn,_) -> float mn
let maxP = function F(_,_,mx) -> mx | I(_,_,mx) -> float mx

let normalizeParm (parmDefs:Parm[]) indv s i  {ParmLo=pLo; ParmHi=pHi} parm = 
    if pLo < parm &&  pHi > parm  then 
        evolveP s eSigma indv i parmDefs.[i] parm
    else
        distributParm s indv i parmDefs.[i] pLo pHi

let createNorms parmDefs isBetter = parmDefs |> Array.map (fun p -> 
    {
        FitnessLo = if isBetter 1. 2. then System.Double.MaxValue else System.Double.MinValue
        ParmLo    =  minP p
        FitnessHi = if isBetter 1. 2. then System.Double.MaxValue else System.Double.MinValue
        ParmHi    = maxP p
    })


let create parmDefs isBetter =
    let create (norms:Norm array) fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Normative
            Accept      = fAccept fInfluence norms
            Influence   = fInfluence norms
        }

    let rec acceptance fInfluence norms envChanged (voters:Individual<_> array) =
        //assumes that individuals are sorted best fitness first
        let norms = if envChanged then createNorms parmDefs isBetter else norms
        let updatedNorms = voters |> Array.fold (updateNorms isBetter) norms
        //printfn "%A" updatedNorms
        voters,create updatedNorms acceptance fInfluence 
    
    let influence (norms:Norm array) s (ind:Individual<_>) =
        (norms,ind.Parms) ||> Array.iteri2 (fun i n p -> normalizeParm parmDefs ind.Parms s i n p)
        ind
        
    let initialNorms = createNorms parmDefs isBetter
    create initialNorms acceptance influence
