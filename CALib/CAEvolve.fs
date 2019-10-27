///Utility functions to support evolution of value
module CAEvolve
open CA
open CAUtils

///Increase parameter value given slope and step size
let stepUp stepSize slope parmDef v = 
    let z' = stepSize * slope
    //printfn "Up: %f * %f = %f + %f" stepSize slope z' v
    match parmDef with
    | F (_,mn,mx)   -> v + z' |> clamp mn mx
    | I (_,mn,mx)   -> v + z' |> clampI mn mx

///Decrease parameter value given slope and step size
let stepDown stepSize slope parmDef v =
    let z' = stepSize * slope
    //printfn "Dn: %f * %f = %f - %f" stepSize slope z' v
    match parmDef with
    | F (_,mn,mx)   -> v - z' |> clamp mn mx
    | I (_,mn,mx)   -> v - z' |> clampI mn mx

///Return parameter range from min and max of Parm type
let range = function
    | F(_,mn,mx)   ->  mn-mx
    | I(_,mn,mx)   ->  mx-mn |> float

///Evolve parameter using Gaussian sampling
///but taking the parameter range into consideration
let evolveS' rangeScaler range influenceLevel sigma v = 
    assert (influenceLevel > 0.0 && influenceLevel <= 5.0)
    assert (sigma > 0.0 && sigma <= 1.0)
    let z = zsample() * range * rangeScaler // pick something in proportion to the allowable range
    let z' = z * influenceLevel * sigma
    //printfn "evolveS' infL=%f; s=%f; v=%f - %f" influenceLevel sigma v z'
    v - z'

///How much of the available parameter range to use for evolving parm
let RANGE_SCALER = 0.25 

///Evolve the ith parameter of the 'influenced' individual where
///influenceLevel is the aggressiveness of the move (a factor that may change over time),
///sigma is a constant that is typically tied to a Knowledge source and controls its exploratory factor,
///parm is the Parm type associated with the parameter at that index,
///pV is the current parameter value of the 'influenced' individual,
///iV is the current parameter value of the 'influencer' individual
let private influenceInd' (influenced:float[]) influenceLevel sigma i parm pV iV =
    //mutation
    match parm,pV,iV with 
    | F(_,_,_),pV,iV when pV > iV -> influenced.[i] <- unifrmF influenceLevel iV pV
    | F(_,_,_),pV,iV when pV < iV -> influenced.[i] <- unifrmF influenceLevel pV iV   
    | F(_,mn,mx),pV,_             -> 
      let r = range parm
      influenced.[i] <- evolveS' RANGE_SCALER  r influenceLevel sigma pV   |> clamp mn mx

    | I(_,mn,mx),pV,iV when pV > iV -> influenced.[i] <- unifrmF influenceLevel iV pV     |> clampI mn mx
    | I(_,mn,mx),pV,iV when pV < iV -> influenced.[i] <- unifrmF influenceLevel pV iV     |> clampI mn mx
    | I(_,mn,mx),pV,_               -> 
      let r = range parm
      influenced.[i] <- evolveS' RANGE_SCALER r influenceLevel sigma pV |> clampI mn mx

///Evolve parameter using Gaussian sampling
///influenceLevel is the aggressiveness of the move (a factor that may change over time),
///sigma is a constant that is typically tied to a Knowledge source and controls its exploratory factor,
///parm is the Parm type associated with the parameter at that index,
///pV is the current parameter value of the individual
let evolveP rangeScaler influenceLevel sigma (indv:float[]) i parm pV =
    //mutation
    let r = range parm
    match parm with
    | F(_,mn,mx)   -> indv.[i] <- evolveS' rangeScaler r influenceLevel sigma pV |> clamp mn mx
    | I(_,mn,mx)   -> indv.[i] <- evolveS' rangeScaler r influenceLevel sigma pV |> clampI mn mx

///Update the ith parameter of the indvidual to be between pLo and pHi using uniform sampling
let distributParm influeceLevel (indv:float[]) i  parm pLo pHi =
    //mutation
    match parm with
    | F(_,mn,mx) -> indv.[i] <- unifrmF influeceLevel pLo pHi 
    | I(_,mn,mx) -> indv.[i] <- unifrmF influeceLevel pLo pHi |> clampI mn mx

///Influenced indivual's parameters are modified 
///to move them towards the influencer's parameters. 
///caParms is the array of Parm types associated with each of the parameters of the individual
///influenceLevel is the aggressiveness of the move (a factor that may change over time),
///sigma is a constant that is typically tied to a Knowledge source and controls its exploratory factor,
let influenceInd caParms influenceLevel sigma (influenced:Individual<_>) (parmsInfluencer:float[]) = 
    let pId = influenced.Parms
    caParms |> Array.iteri (fun i p -> influenceInd' pId influenceLevel sigma i p pId.[i] parmsInfluencer.[i])
    influenced

///Evolve parameters of an individual using Gaussian sampling
///caParms is the array of Parm types associated with each of the parameters of the individual
///influenceLevel is the aggressiveness of the move (a factor that may change over time),
///sigma is a constant that is typically tied to a Knowledge source and controls its exploratory factor,
let evolveInd rangeScaler caParms influenceLevel sigma (individual:Individual<_>) =
    let prms = individual.Parms
    caParms |> Array.iteri (fun i p -> evolveP rangeScaler influenceLevel sigma prms i p prms.[i])
    individual
