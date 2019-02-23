﻿module NormativeKS
open CA
open CAUtils
open CAEvolve

let eSigma = 1.0

type Norm = 
    {
        FitnessLo : float
        ParmLo    : float
        FitnessHi : float
        ParmHi    : float
    }

type NormState = 
    {
        Norms       : Norm[]
        IsBetter    : Comparator
        ParmDefs    : Parm[]
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
        evolveP CAEvolve.RANGE_SCALER s eSigma indv i parmDefs.[i] parm
    else
        distributParm s indv i parmDefs.[i] pLo pHi

let createNorms parmDefs isBetter = parmDefs |> Array.map (fun p -> 
    {
        FitnessLo = if isBetter 1. 2. then System.Double.MaxValue else System.Double.MinValue
        ParmLo    =  minP p
        FitnessHi = if isBetter 1. 2. then System.Double.MaxValue else System.Double.MinValue
        ParmHi    = maxP p
    })

let log (norms:Norm[]) =
  let low = norms |> Array.map (fun n -> n.ParmLo)
  let hi = norms |> Array.map (fun n -> n.ParmHi)
  [low;hi] |> Metrics.NormState |> Metrics.postAll

let construct state fAccept fInfluence : KnowledgeSource<_> =
    {
        Type        = Normative
        Accept      = fAccept fInfluence state
        Influence   = fInfluence state
    }

let initialState parmDefs isBetter = {ParmDefs=parmDefs; IsBetter=isBetter; Norms=createNorms parmDefs isBetter}

let defaultInfluence {Norms=norms; ParmDefs=parmDefs} s (ind:Individual<_>) =
    (norms,ind.Parms) ||> Array.iteri2 (fun i n p -> normalizeParm parmDefs ind.Parms s i n p)
    ind

let rec defaultAcceptance fInfluence state envChanged (voters:Individual<_> array) =

    //assumes that individuals are sorted best fitness first
    let norms = 
        match Settings.TrackEnv, envChanged with
        | true, true -> createNorms state.ParmDefs state.IsBetter
        | _          -> state.Norms

    let updatedNorms = voters |> Array.fold (updateNorms state.IsBetter) norms
    //printfn "%A" updatedNorms
    #if _LOG_
    log updatedNorms
    #endif
    let state = {state with Norms=updatedNorms}
    voters,construct state defaultAcceptance fInfluence 

let create parmDefs isBetter =    
    let state = initialState parmDefs isBetter
    construct state defaultAcceptance defaultInfluence
