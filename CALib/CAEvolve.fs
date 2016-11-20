module CAEvolve
open CA
open CAUtils

let slideUp s = function
    | F (v,mn,mx)   -> F (randF s v mx mn mx, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 s v mx mn mx, mn, mx)
    | I (v,mn,mx)   -> I(randI s v mx mn mx, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 s v mx mn mx, mn, mx)

let slideDown s = function
    | F (v,mn,mx)   -> F (randF s mn v mn mx, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 s mn v mn mx, mn, mx)
    | I (v,mn,mx)   -> I(randI s mn v mn mx, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 s mn v mn mx, mn, mx)

let evolveInt s iV =
    let v = float iV
    let v' = gaussian v (s * 3.)
    if abs (v' - v) > 1. then 
        int v' 
    elif v'<v then 
        iV - 1 
    else 
        iV + 1 

let evolveInt64 s i64V =
    let v  = float i64V
    let v' = gaussian v (s * 3.)
    if abs (v' - v) > 1. then 
        int64 v' 
    elif v' < v then 
        i64V - 1L
    else 
        i64V + 1L 

let evolveS s = function
    | F (v,mn,mx)    -> F   (gaussian v (s * 1.)                        |> clamp mn mx, mn, mx)
    | F32 (v,mn,mx)  -> F32 (gaussian  (float v) (s * 1.) |> float32    |> clamp mn mx, mn, mx)
    | I (v,mn,mx)    -> I   (evolveInt s v                              |> clamp mn mx, mn, mx)
    | I64 (v,mn,mx)  -> I64 (evolveInt64 s v                            |> clamp mn mx, mn, mx)

///Use values from the 2nd parm to influence 1st parm
///(randomly move towards 2nd parm value)
let influenceParm s influenced influencer =
    match influencer,influenced with
    | F(pV,mn,mx),F(iV,_,_) when pV > iV     -> F(randF s iV pV mn mx, mn,mx)
    | F(pV,mn,mx),F(iV,_,_) when pV < iV     -> F(randF s pV iV mn mx, mn,mx)
    | F(_),fInd                              -> evolveS s fInd

    | F32(pV,mn,mx),F32(iV,_,_) when pV > iV -> F32(randF32 s iV pV mn mx,mn,mx)
    | F32(pV,mn,mx),F32(iV,_,_) when pV < iV -> F32(randF32 s pV iV mn mx,mn,mx)
    | F32(_),fInd                            -> evolveS s fInd

    | I(pV,mn,mx),I(iV,_,_) when pV > iV     -> I(randI s iV pV mn mx,mn,mx)
    | I(pV,mn,mx),I(iV,_,_) when pV < iV     -> I(randI s pV iV mn mx,mn,mx)
    | I(_),fInd                              -> evolveS s fInd

    | I64(pV,mn,mx),I64(iV,_,_) when pV > iV -> I64(randI64 s iV pV mn mx,mn,mx)
    | I64(pV,mn,mx),I64(iV,_,_) when pV < iV -> I64(randI64 s pV iV mn mx,mn,mx)
    | I64(_),fInd                            -> evolveS s fInd

    | a,b -> failwithf "two pop individual parameters not matched %A %A" a b

///influenced indivual's parameters are modified 
///to move them towards the influencer's parameters
let influenceInd s influenced influencer =
    {influenced with
        Parms = (influenced.Parms,influencer.Parms) ||> Array.map2  (influenceParm s)
    }

///influenced indivual's parameters are modified 
///to move them towards the influencer's parameters
let evolveInd s individual =
    {individual with
        Parms = individual.Parms |> Array.map (evolveS s)
    }

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