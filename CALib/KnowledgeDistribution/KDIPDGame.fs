module KDIPDGame
open CA
open CAUtils
open KDContinousStrategyGame

let NEW_KS_LEVEL            = 1.0 //influence level for a new primary KS
let MAX_ALT_KS_INFLUENCE    = 1.0 //cap influence of secondary ks 
//let KS_ADJUST             = 0.9 //adjustment mutliplier to influence level if indiv keeps same KS next gen
let SCNRY_EXPL_KS_BOOST     = 0.9 //aggressiveness boost for secondary exploitative KS
let MIN_INFLUENCE_LEVEL     = 0.001 //floor for influence level
let LOST_KS_WT = 0.20 // percentage of pop that is randomly assigned a KS that was pushed out

type PrimaryKS = {KS:Knowledge; Level:float}
type IpdKS = PrimaryKS * Map<Knowledge,float> //each indv has a primary ks and partial influece KSs
type W = Set<Id> * float
type Action = W list
type Payout = Action

let primKS ({KS=ks;Level=_},_) = ks

//for logging
open TracingGame
let obsNetW,fpNetW = Observable.createObservableAgent<SG<IpdKS>> Tracing.cts.Token


type Ada =
    | Fixed of float
    | Geometric of (float*float) // multiplier (1.0-0.0) * minimum
    | Linear of (float*float)    // decrement * minimum

type IpdState = 
    {
        SumDiversity    : float
        Gen             : int
        NormalizedFit   : float array
        PrevNrmlzdFit   : float array
        VMin            : float
        VMax            : float
        Stability       : float array
        KSCount         : Map<Knowledge,float>
        KSAdjust        : Ada 
        ExploitativeKS  : Knowledge
        KSSet           : Set<Knowledge>
        }

let parmDiversity p1 p2 = 
    (p1,p2)
    ||> Array.map2 (fun a b -> a - b |> abs) 
    |> Array.sum

//est. avg diversity of pop via sampling
let sampleAvgDiversity (pop:Population<_>) =
    let sampleSize = 30
    let rec loop acc c =
        if c >= sampleSize 
            then acc / float sampleSize
        else
            let i1 = rnd.Value.Next(0,pop.Length-1)
            let i2 = rnd.Value.Next(0,pop.Length-1)
            let p1 = pop.[i1]
            let p2 = pop.[i2]
            let d = parmDiversity p1.Parms p2.Parms
            loop (acc + d) (c + 1)
    loop 0. 0

let isExploitative state ks = state.ExploitativeKS = ks
let isExplorative state ks = isExploitative state ks |> not

//let KS_ATTRACTION_COFF      = 1.5 //attraction between exploitative and explorative KS
//let IMPROVE_DEFECT_COFF     = -0.7 //reduction in cooperation with others due to improved fit from last gen of exploitative ks
//let LOW_KS_COUNT_EXPONENT   = 3.0 //factor to prevent a low count KS from being pushed out
//let FIT_ATTRACTION_WEIGHT   = 2.0 //weight for supperior fitness term 
//let STABILITY_WEIGHT        = 0.01 //weight for stability (of same KS from gen-to-gen) factor in cooperation
//let DIVERSITY_WEIGHT        = 2.0 //weigt given to diversity

type Coeff = {
  KS_ATTRACTION_COFF      : float //attraction between exploitative and explorative KS
  IMPROVE_DEFECT_COFF     : float //reduction in cooperation with others due to improved fit from last gen of exploitative ks
  LOW_KS_COUNT_EXPONENT   : float //factor to prevent a low count KS from being pushed out
  FIT_ATTRACTION_WEIGHT   : float //weight for supperior fitness term 
  STABILITY_WEIGHT        : float //weight for stability (of same KS from gen-to-gen) factor in cooperation
  DIVERSITY_WEIGHT        : float //weigt given to diversity
}

let defCoeff =
  {
    KS_ATTRACTION_COFF      = 1.5 //attraction between exploitative and explorative KS
    IMPROVE_DEFECT_COFF     = -0.7 //reduction in cooperation with others due to improved fit from last gen of exploitative ks
    LOW_KS_COUNT_EXPONENT   = 3.0 //factor to prevent a low count KS from being pushed out
    FIT_ATTRACTION_WEIGHT   = 2.0 //weight for supperior fitness term 
    STABILITY_WEIGHT        = 0.01 //weight for stability (of same KS from gen-to-gen) factor in cooperation
    DIVERSITY_WEIGHT        = 2.0 //weigt given to diversity
  }

let explorativeCoeffs =
  {
    KS_ATTRACTION_COFF      = 2.0 //attraction between exploitative and explorative KS
    IMPROVE_DEFECT_COFF     = -0.3 //reduction in cooperation with others due to improved fit from last gen of exploitative ks
    LOW_KS_COUNT_EXPONENT   = 3.0 //factor to prevent a low count KS from being pushed out
    FIT_ATTRACTION_WEIGHT   = 2.0 //weight for supperior fitness term 
    STABILITY_WEIGHT        = 0.01 //weight for stability (of same KS from gen-to-gen) factor in cooperation
    DIVERSITY_WEIGHT        = 3.0 //weigt given to diversity
  }

let cooperation 
    coeffs
    state
    neighbor 
    indv =  
    let ids = set[indv.Id; neighbor.Id]
    let d = parmDiversity indv.Parms neighbor.Parms / state.SumDiversity  //normalize diversity
    let fNbr = state.NormalizedFit.[neighbor.Id]
    let fI = state.NormalizedFit.[indv.Id]
    let pf1I = state.PrevNrmlzdFit.[indv.Id]
    let stability = state.Stability.[indv.Id]
    let ({KS=ksI;Level=_},md) = indv.KS
    let ({KS=ksN;Level=_},_) = neighbor.KS
    let nKSC = 1. - state.KSCount.[ksN] //level of ks
    let coop =
        let attraction = (fNbr - fI) // |> max 0.
        let defectCoof =  if isExploitative state ksI && (fI > pf1I) then coeffs.IMPROVE_DEFECT_COFF else 0.
        let ksCompatibility = if isExplorative state ksI && isExploitative state ksN then coeffs.KS_ATTRACTION_COFF else 0.
        let sameKSDefection = if ksI = ksN then -3.0 else 0.
        let kslow = nKSC ** coeffs.LOW_KS_COUNT_EXPONENT
        let fitattraction = (attraction * coeffs.FIT_ATTRACTION_WEIGHT)
        let diversity = d * coeffs.DIVERSITY_WEIGHT
        let stability = stability * coeffs.STABILITY_WEIGHT
        let c = ksCompatibility  + defectCoof  + kslow + fitattraction + stability + diversity + sameKSDefection
//        {id1=indv.Id; id2=neighbor.Id; attr=attr; 
//         def=defectCoof; kscom=ksCompatibility;
//         kslow=kslow; dv=d; gen=state.Gen; coop=c;
//         stb=stability;
//         ksI=ksI; ksN=ksN } 
//        |> MCoop |> log.Post
        c
    ids,coop

let relativeCoop totalCoop (s,coop) = s, if coop < 0. then 0. else coop / totalCoop

let play state _ indv neighbors payoff : Action =
    let coops = neighbors |> Seq.map (cooperation defCoeff state indv) |> Seq.toList
    let sumCoops = coops |> List.sumBy (fun (_,c) -> max c 0.)
    let ws = coops |> List.map (relativeCoop sumCoops)
    ws

let payoff _ _ indv indvActn (nhbrActns:Action seq) : Payout =
    let m1 = indvActn |> Map.ofList
    let payoff = 
        nhbrActns 
        |> Seq.choose (fun acts -> 
            acts 
            |> List.tryPick (fun (s,c1) ->
                m1 
                |> Map.tryFind s
                |> Option.map (fun c2 -> s, c1+c2))
            )
        |> Seq.toList
    if payoff.Length <> (Seq.length nhbrActns) then
        failwithf "neighbor action counts not matched %A %A" indv nhbrActns
    payoff


let other i s = if Set.minElement s = i then Set.maxElement s else Set.minElement s


let updtScndryKS (pop:Population<IpdKS>) payout indv =
    let ksw = 
        payout 
        |> List.map (fun (idSet,f) ->  //set<id>, float [0-2]
            let nhbrId = other indv.Id idSet
            let pks = primKS pop.[nhbrId].KS
            pks,f / 2.)    //payout is in range [0., 2.] so normalize to max 1.0
        |> List.groupBy fst
        |> List.map (fun (ks,fs)->
            ks,
            fs |> List.sumBy snd |> min MAX_ALT_KS_INFLUENCE
            )
        |> Map.ofList
    ksw

let removePrimaryKS ks m = Map.remove ks m
let removeSituationalKS m = Map.remove Situational m //Situational cannot be secondary KS as it changes the indv completely
let promoteDomainKS : IpdKS->IpdKS = function ({KS=Domain;Level=_} as pks,_) -> (pks,Map.empty) | k -> k 

//remove secondary explorative KS if primary is explorative
let removeExpScndryKS state primaryKS m = if isExplorative state primaryKS then  m |> Map.filter (fun k _ -> isExplorative state k |> not) else m

let logI st newKs indv = 
    let ps = indv.Parms 
    let pksOld = primKS indv.KS
    let pksNew = primKS newKs
    {id=indv.Id; x=ps.[0]; y=ps.[1]; ks=pksNew; ksp=pksOld; gen=st.Gen; fit=indv.Fitness;} 
    |> MIndv |> fpGame

let rate r = function 
    | Fixed f           -> f 
    | Geometric (mu,mn) -> r * mu |> max mn
    | Linear (delta,mn) -> r - delta |> max mn

let createWthKS st (indv:Individual<IpdKS>) primary secondary = 
    let {KS=oldKS;Level=oldLvl} = fst indv.KS

    let ({KS=newKS;Level=_},secondary) = 
        (
          primary, 
          removePrimaryKS primary.KS secondary 
          |> removeSituationalKS 
          |> removeExpScndryKS st primary.KS
        ) 
        |> promoteDomainKS

    let primary =
        if newKS = oldKS && isExploitative st newKS then
            {KS=newKS; Level= rate oldLvl st.KSAdjust}
        else
            {KS=newKS; Level=NEW_KS_LEVEL}
    let ks = primary,secondary
//    logI st ks indv
    {indv with KS=ks}

let updateIndv st (vmin,vmax) cmprtr (pop:Population<IpdKS>) (indv:Individual<IpdKS>) payout =
//    payout |> List.iter (fun (is,f) -> {gen=st.Gen; idA=Set.minElement is; idB=Set.maxElement is; payoff=f} |> MPout |> log.Post)
    let payout = payout |> List.filter (fun (_,f) -> f > vmin)
    let vmx = payout |> List.filter (fun (_,f) -> f >= vmax)
    let indv = 
        match vmx.Length,payout.Length with
        | 0,0 -> createWthKS st indv (fst indv.KS) Map.empty
        | 0,_ -> createWthKS st indv (fst indv.KS) (updtScndryKS pop payout indv)
        | 1,_ ->
            let nhbr = pop.[other indv.Id (fst vmx.[0])]
            let (pks,_):IpdKS = nhbr.KS
            let (oldPks,_) = indv.KS
            //if (pks.KS = oldPks.KS) then printfn "same ks %A" pks.KS
            createWthKS st indv pks Map.empty
        | _,_ ->  
            let i = CAUtils.rnd.Value.Next(0,vmx.Length - 1)
            let nhbr = pop.[other indv.Id (fst vmx.[i])]
            let (pks,_):IpdKS = nhbr.KS
            createWthKS st indv pks Map.empty
    indv

let updatePop st vmx cmprtr (pop:Population<IpdKS>) (payouts:Payout array) = 
    let pop = pop |> Array.Parallel.map(fun indv ->
        updateIndv st vmx cmprtr pop indv payouts.[indv.Id]
    )
    pop

let private ipdInfluence state beliefSpace (pop:Population<IpdKS>) =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> dict
    let pop =
        pop
        |> Array.Parallel.map (fun p -> 
        //|> Array.map (fun p -> 
            let {KS=mainKS;Level=lvl},otherKs = p.KS
            let beforeP = Array.copy p.Parms
            let p = ksMap.[mainKS].Influence lvl p
            let p = (p,otherKs) ||> Map.fold (fun p k w -> if k <> state.ExploitativeKS then ksMap.[k].Influence w p else p) //explorative ks go first
            let p = (p,otherKs) ||> Map.fold (fun p k w -> if k = state.ExploitativeKS then ksMap.[k].Influence (w*SCNRY_EXPL_KS_BOOST) p else p) //exploitative ks go last
            //if p.Parms = [|1.0; 1.0|] then
            //  let ps = p.Parms
            //  ()
            p)
    //let oneP = pop |> Array.filter (fun i->i.Parms =[| 1.0; 1.0 |]) |> Array.length
    //let zeroP = pop |> Array.filter (fun i->i.Parms =[| 0.0; 0.0 |]) |> Array.length
    //printfn "1=%d; 0=%d; t=%d" oneP zeroP pop.Length
    pop 

let createState exploitativeKS ksAdjust gen stability prevFitOpt (vmin,vmax) cmprtr (pop:Population<IpdKS>) =
    let nrmlzdFit = normalizePopFitness (0., 1.0) cmprtr pop
    let ksc = 
        pop 
//        |> Seq.collect(fun i-> let ({KS=ks;Level=l},m) = i.KS in (ks,l)::(Map.toList m))  
        |> Seq.map(fun i-> let pks = primKS i.KS in pks,1.0)  
        |> Seq.groupBy fst 
        |> Seq.map (fun (k,xs)->k, xs |>Seq.sumBy snd)
    let totalKsc = ksc |> Seq.sumBy snd
    let ksc = ksc |> Seq.map (fun (k,v) -> k, v / totalKsc) |> Map.ofSeq
    let ksSet = ksc |> Map.toSeq |> Seq.map fst |> set
    {
        SumDiversity = sampleAvgDiversity pop * float pop.Length
        NormalizedFit = nrmlzdFit
        VMin = vmin
        VMax = vmax
        PrevNrmlzdFit = match prevFitOpt with Some (f,_) -> f | None -> nrmlzdFit
        Stability = stability
        KSCount = ksc
        Gen = gen + 1
        KSAdjust = ksAdjust
        ExploitativeKS = exploitativeKS
        KSSet = ksSet
    }

let updateStability f = function
    | Domain,Domain -> f + 1. //|> min 5.
    | _,_           -> 0.

//let logNetwork popSz state (payouts:Payout array) = TracingGame.fpNetW({popSz, state.VMin,payouts)


let rec outcome state envCh optKind (pop,beliefSpace,network) (payouts:Payout array) =
    let vmx = (state.VMin, state.VMax)
    let mult = CAUtils.mult optKind
    let pop' = updatePop state vmx mult pop payouts
    let stability = state.Stability |> Array.mapi (fun i f -> updateStability f (primKS pop'.[i].KS, primKS pop.[i].KS))
    let pop = ipdInfluence state beliefSpace pop'
    let state = createState state.ExploitativeKS state.KSAdjust state.Gen stability (Some (state.NormalizedFit,state.PrevNrmlzdFit)) vmx mult pop
  
    #if _LOG_
    fpNetW {Pop=pop';Net=network; Vmin=state.VMin; Links=payouts}
    #endif
    let pop =
        if  state.Gen % 5 = 0 then
            let missingKS = pop |> Array.Parallel.map (fun i -> primKS i.KS) |> set |> Set.difference state.KSSet
            for ks in missingKS do
                for _ in 1 .. (float pop.Length * LOST_KS_WT |> int) do
                    let indx = CAUtils.rnd.Value.Next(pop.Length)
                    //mutate local copy of pop array
                    pop.[indx] <- createWthKS state  pop.[indx] {KS=ks;Level=1.0} Map.empty 
            pop
        else
            pop

    pop,
    beliefSpace,
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state
    }

let initKS (pop:Population<Knowledge>) : Population<IpdKS> = 
    pop 
    |> Array.Parallel.map (fun indv -> 
        {
            Id = indv.Id
            Fitness = indv.Fitness
            Parms = indv.Parms
            KS={KS=indv.KS;Level=NEW_KS_LEVEL},Map.empty
        })

let game exploitativeKS ksAdjust  (vmin,vmax) cmprtr (pop:Population<IpdKS>) =
    let stability = Array.zeroCreate pop.Length
    let state = createState exploitativeKS ksAdjust 0 stability None (vmin,vmax) cmprtr pop
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state
    }

let influence exploitativeKs ksAdjust (vmin,vmax) optKind pop =
    let g = game exploitativeKs ksAdjust (vmin,vmax)  (CAUtils.mult optKind) pop
    KDContinousStrategyGame.influence g

