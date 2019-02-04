module CAEvolve
open CA
open CAUtils

let stepUp stepSize slope parmDef v = 
    let z' = stepSize * slope
    //printfn "Up: %f * %f = %f + %f" stepSize slope z' v
    match parmDef with
    | F (_,mn,mx)   -> v + z' |> clamp mn mx
    | I (_,mn,mx)   -> v + z' |> clampI mn mx

let stepDown stepSize slope parmDef v =
    let z' = stepSize * slope
    //printfn "Dn: %f * %f = %f - %f" stepSize slope z' v
    match parmDef with
    | F (_,mn,mx)   -> v - z' |> clamp mn mx
    | I (_,mn,mx)   -> v - z' |> clampI mn mx

//let slideUp influenceLevel sigma parmDef v = 
//    let z = zsample()
//    let z' = z * influenceLevel * sigma |> abs
//    match parmDef with
//    | F (_,mn,mx)   -> v + z' |> clamp mn mx
//    | I (_,mn,mx)   -> v + z' |> clampI mn mx

//let slideDown influenceLevel sigma parmDef v =
//    let z = zsample()
//    let z' = z * influenceLevel * sigma |> abs
//    match parmDef with
//    | F (_,mn,mx)   -> v - z' |> clamp mn mx
//    | I (_,mn,mx)   -> v - z' |> clampI mn mx

let range = function
    | F(_,mn,mx)   ->  mn-mx
    | I(_,mn,mx)   ->  mx-mn |> float

let evolveS' rangeScaler range influenceLevel sigma v = 
    assert (influenceLevel > 0.0 && influenceLevel <= 5.0)
    assert (sigma > 0.0 && sigma <= 1.0)
    let z = zsample() * range * rangeScaler // pick something in proportion to the allowable range
    let z' = z * influenceLevel * sigma
    //printfn "evolveS' infL=%f; s=%f; v=%f - %f" influenceLevel sigma v z'
    v - z'

let RANGE_SCALER = 0.25 //how much of the available parameter range to use for evolving parm

let private influenceInd' (influenced:float[]) influenceLevel sigma i parm pV iV =
    //mutation
    match parm,pV,iV with 
    | F(_,_,_),pV,iV when pV > iV -> influenced.[i] <- unifrmF influenceLevel iV pV
    | F(_,_,_),pV,iV when pV < iV -> influenced.[i] <- unifrmF influenceLevel pV iV       //TODO: scaling not symmetrical in both directions
    | F(_,mn,mx),pV,_             -> 
      let r = range parm
      influenced.[i] <- evolveS' RANGE_SCALER  r influenceLevel sigma pV   |> clamp mn mx

    | I(_,mn,mx),pV,iV when pV > iV -> influenced.[i] <- unifrmF influenceLevel iV pV     |> clampI mn mx
    | I(_,mn,mx),pV,iV when pV < iV -> influenced.[i] <- unifrmF influenceLevel pV iV     |> clampI mn mx
    | I(_,mn,mx),pV,_               -> 
      let r = range parm
      influenced.[i] <- evolveS' RANGE_SCALER r influenceLevel sigma pV |> clampI mn mx

let evolveP rangeScaler influenceLevel sigma (indv:float[]) i parm pV =
    //mutation
    let r = range parm
    match parm with
    | F(_,mn,mx)   -> indv.[i] <- evolveS' rangeScaler r influenceLevel sigma pV |> clamp mn mx
    | I(_,mn,mx)   -> indv.[i] <- evolveS' rangeScaler r influenceLevel sigma pV |> clampI mn mx

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
let evolveInd rangeScaler caParms influenceLevel sigma (individual:Individual<_>) =
    let prms = individual.Parms
    caParms |> Array.iteri (fun i p -> evolveP rangeScaler influenceLevel sigma prms i p prms.[i])
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