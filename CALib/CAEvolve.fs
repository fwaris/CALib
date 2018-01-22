module CAEvolve
open CA
open CAUtils

let slideUp influenceLevel sigma parmDef v = 
    let z = zsample()
    let z' = z * influenceLevel * sigma |> abs
    match parmDef with
    | F (_,mn,mx)   -> v + z' |> clamp mn mx
    | I (_,mn,mx)   -> v + z' |> clampI mn mx

let slideDown influenceLevel sigma parmDef v =
    let z = zsample()
    let z' = z * influenceLevel * sigma |> abs
    match parmDef with
    | F (_,mn,mx)   -> v - z' |> clamp mn mx
    | I (_,mn,mx)   -> v - z' |> clampI mn mx

let evolveS' influenceLevel sigma v = 
    assert (influenceLevel > 0.0 && influenceLevel <= 1.0)
    assert (sigma > 0.0 && sigma <= 1.0)
    let z = zsample()
    let z' = z * influenceLevel * sigma
    v - z'

let private influenceInd' (influenced:float[]) influenceLevel sigma i parm pV iV =
    //mutation
    match parm,pV,iV with 
    | F(_,_,_),pV,iV when pV > iV -> influenced.[i] <- unifrmF influenceLevel iV pV
    | F(_,_,_),pV,iV when pV < iV -> influenced.[i] <- unifrmF influenceLevel pV iV       //TODO: scaling not symmetrical in both directions
    | F(_,mn,mx),pV,_             -> influenced.[i] <- evolveS' influenceLevel sigma pV   |> clamp mn mx

    | I(_,mn,mx),pV,iV when pV > iV -> influenced.[i] <- unifrmF influenceLevel iV pV     |> clampI mn mx
    | I(_,mn,mx),pV,iV when pV < iV -> influenced.[i] <- unifrmF influenceLevel pV iV     |> clampI mn mx
    | I(_,mn,mx),pV,_               -> influenced.[i] <- evolveS' influenceLevel sigma pV |> clampI mn mx

let evolveP influenceLevel sigma (indv:float[]) i parm pV =
    //mutation
    match parm with
    | F(_,mn,mx)   -> indv.[i] <- evolveS' influenceLevel sigma pV |> clamp mn mx
    | I(_,mn,mx)   -> indv.[i] <- evolveS' influenceLevel sigma pV |> clampI mn mx

let distributParm influeceLevel (indv:float[]) i  parm pLo pHi =
    //mutation
    match parm with
    | F(_,mn,mx) -> indv.[i] <- unifrmF influeceLevel pLo pHi 
    | I(_,mn,mx) -> indv.[i] <- unifrmF influeceLevel pLo pHi |> clampI mn mx

///influenced indivual's parameters are modified 
///to move them towards the influencer's parameters
let influenceInd caParms influenceLevel sigma (influenced:Individual<_>) (parmsInfluencer:float[]) = 
    let pId = influenced.Parms
    caParms |> Array.iteri (fun i p -> influenceInd' pId influenceLevel sigma i p pId.[i] parmsInfluencer.[i])
    influenced

///influenced indivual's parameters are modified 
///to move them towards the influencer's parameters
let evolveInd caParms influenceLevel sigma (individual:Individual<_>) =
    let prms = individual.Parms
    caParms |> Array.iteri (fun i p -> evolveP influenceLevel sigma prms i p prms.[i])
    individual

(*
#load "CA.fs"
#load "CAUtils.fs"
#load "CAEvolve.fs"
open CAEvolve
let r1 = randI 1.5 -1 -5 -10 0
let r1 = randI 1.5 -1 10 -10 20
let r1 = randF 0.8 -1. -5. -10. 0.
let r1 = randF 1.5 -1. 10. -10. 20.
let r1 = randF32 0.8 -1.f -5.f -10.f 0.f
let r1 = randF32 1.5 -1.f 10.f -10.f 20.f
let r1 = randI64 0.8 -1L -5L -10L 0L
let r1 = randI64 1.5 -1L 10L -10L 20L
*)