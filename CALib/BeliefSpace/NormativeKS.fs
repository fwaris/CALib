module NormativeKS
open CA
open CAUtils
open CAEvolve

let eSigma = 1.0

type ParmRange =
    | Fr    of float
    | Fr32  of float32
    | Ir    of int
    | Ir64  of int64

let toRange = function
    | F(v,_,_)   -> Fr(v)
    | F32(v,_,_) -> Fr32(v)
    | I(v,_,_)   -> Ir(v)
    | I64(v,_,_) -> Ir64(v)

let toRangeLo = function
    | F(_,l,_)   -> Fr(l)
    | F32(_,l,_) -> Fr32(l)
    | I(_,l,_)   -> Ir(l)
    | I64(_,l,_) -> Ir64(l)

let toRangeHi = function
    | F(_,_,h)   -> Fr(h)
    | F32(_,_,h) -> Fr32(h)
    | I(_,_,h)   -> Ir(h)
    | I64(_,_,h) -> Ir64(h)

let isLower = function
    | Fr(pL),F(iV,_,_)      when iV > pL -> true
    | Fr32(pL),F32(iV,_,_)  when iV > pL -> true
    | Ir(pL),I(iV,_,_)      when iV > pL -> true
    | Ir64(pL),I64(iV,_,_)  when iV > pL -> true
    | _ -> false

let isHigher = function
    | Fr(pH),F(iV,_,_)      when iV < pH -> true
    | Fr32(pH),F32(iV,_,_)  when iV < pH -> true
    | Ir(pH),I(iV,_,_)      when iV < pH -> true
    | Ir64(pH),I64(iV,_,_)  when iV < pH -> true
    | _ -> false

type Norm = 
    {
        FitnessLo : float
        ParmLo    : ParmRange
        FitnessHi : float
        ParmHi    : ParmRange
    }

let updateNorms isBetter norms highPerfInd =
    norms 
    |> Array.mapi (fun i ({FitnessLo=fLo; ParmLo=pLo; FitnessHi=fHi; ParmHi=pHi} as norm) ->
        let parm = highPerfInd.Parms.[i]
        match 
            isBetter highPerfInd.Fitness fLo, 
            isBetter highPerfInd.Fitness fHi with
        | true,true ->
            {
                FitnessLo  = highPerfInd.Fitness
                ParmLo     = toRange parm
                FitnessHi  = highPerfInd.Fitness
                ParmHi     = toRange parm
            }
        | true,false ->
            let isPrevHigher = isHigher (pHi,parm)
            {
                FitnessLo  = highPerfInd.Fitness
                ParmLo     = toRange parm
                FitnessHi  = if isPrevHigher then fHi else highPerfInd.Fitness
                ParmHi     = if isPrevHigher then pHi else toRange parm
            }
        | false,true ->
            let isPrevLower = isLower (pLo,parm)
            {
                FitnessLo  = if isPrevLower then fLo else highPerfInd.Fitness
                ParmLo     = if isPrevLower then pLo else toRange parm
                FitnessHi  = highPerfInd.Fitness
                ParmHi     = toRange parm
            }
        | false,false ->
            let isPrevHigher = isHigher (pHi,parm)
            let isPrevLower  = isLower (pLo,parm)
            {
                FitnessLo  = if isPrevLower then fLo else highPerfInd.Fitness
                ParmLo     = if isPrevLower then pLo else toRange parm
                FitnessHi  = if isPrevHigher then fHi else highPerfInd.Fitness
                ParmHi     = if isPrevHigher then pHi else toRange parm
            }
    )

let normalizeParm s {ParmLo=pLo; ParmHi=pHi} parm = 
    if isLower (pLo,parm) && isHigher(pHi,parm) then evolveS s eSigma parm
    else
        match parm,pLo,pHi with
        | F(_,mn,mx),Fr(l),Fr(h)        -> F(unifrmF s l h mn mx,mn,mx)
        | F32(_,mn,mx),Fr32(l),Fr32(h)  -> F32(unifrmF32 s l h mn mx,mn,mx)
        | I(_,mn,mx),Ir(l),Ir(h)        -> I(unifrmI s l h mn mx,mn,mx)
        | I64(_,mn,mx),Ir64(l),Ir64(h)  -> I64(unifrmI64 s l h mn mx,mn,mx)
        | a,b,c -> failwithf "Normative: norm-parameter type mismatch %A,%A,%A" a b c

let create parms isBetter =
    let create (norms:Norm array) fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Normative
            Accept      = fAccept fInfluence norms
            Influence   = fInfluence norms
        }

    let rec acceptance fInfluence norms (voters:Individual<_> array) =
        //assumes that individuals are sorted best fitness first
        let updatedNorms = voters |> Array.fold (updateNorms isBetter) norms
        //printfn "%A" updatedNorms
        voters,create updatedNorms acceptance fInfluence 
    
    let influence (norms:Norm array) s (ind:Individual<_>) =
        {ind with
            Parms = (norms,ind.Parms) ||> Array.map2 (normalizeParm s)
        }
        
    let initialNorms = parms |> Array.map (fun p -> 
        {
            FitnessLo = if isBetter 1. 2. then System.Double.MaxValue else System.Double.MinValue
            ParmLo    = toRangeLo p
            FitnessHi = if isBetter 1. 2. then System.Double.MaxValue else System.Double.MinValue
            ParmHi    = toRangeHi p
        })

    create initialNorms acceptance influence
