module KDIPDGame
open CA
open CAUtils
open KDContinousStrategyGame
open FSharp.Collections.ParallelSeq

//logging code - not functional
type LIndv = {id:int; x:float; y:float; ks:Knowledge; ksp:Knowledge; gen:int; fit:float}
type LCoop = {id1:int; id2:int; attr:float; def:float; 
              kscom:float; kslow:float; dv:float; gen:int; coop:float
              stb:float;
              ksI:Knowledge; ksN:Knowledge}
type LPout = {gen:int; idA:int; idB:int; payoff:float }
type Log = MIndv of LIndv | MCoop of LCoop | MPout of LPout
let mutable ld = []
let log : MailboxProcessor<Log> = MailboxProcessor.Start(fun inbox -> 
    async {
        while true do
            let! msg = inbox.Receive()
            ld <- [msg]
//            ld <- msg::ld //use this to keep logging data
    })

let NEW_KS_LEVEL            = 1.0 //influence level for a new primary KS
let MAX_ALT_KS_INFLUENCE    = 1.0 //cap influence of secondary ks 
let KS_ADJUST               = 0.9 //adjustment mutliplier to influence level if indiv keeps same KS next gen
let SCNRY_EXPL_KS_BOOST     = 0.9 //aggressiveness boost for secondary exploitative KS
let MIN_INFLUENCE_LEVEL     = 0.001 //floor for influence level

type PrimaryKS = {KS:Knowledge; Level:float}
type IpdKS = PrimaryKS * Map<Knowledge,float> //each indv has a primary ks and partial influece KSs
type W = Set<Id> * float
type Action = W list
type Payout = Action

type IpdState = 
    {
        SumDiversity    : float
        Gen             : int
        NormalizedFit   : float array
        VMin            : float
        VMax            : float
        P1Fit           : float array
        Stability       : float array
        KSCount         : Map<Knowledge,float>
        }

let parmDiversity p1 p2 = 
    (p1,p2)
    ||> Array.map2 (fun a b -> parmDiff a b |> parmToFloat |> abs) 
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

let isExplorative = function Situational | Historical | Normative -> true | _ -> false
let isExploitative = function Domain  -> true | _ -> false

let KS_ATTRACTION_COFF      = 1.0 //attraction between exploitative and explorative KS
let IMPROVE_DEFECT_COFF     = -0.7 //reduction in cooperation with others due to improved fit from last gen of exploitative ks
let LOW_KS_COUNT_EXPONENT   = 3.0 //factor to prevent a low count KS from being pushed out
let FIT_ATTRACTION_WEIGHT   = 2.0 //weight for supperior fitness term 
let STABILITY_WEIGHT        = 0.01 //weight for stability (of same KS from gen-to-gen) factor in cooperation
let DIVERSITY_WEIGHT        = 2.0 //weigt given to diversity


let cooperation 
    state
    neighbor 
    indv =  
    let ids = set[indv.Id; neighbor.Id]
    let d = parmDiversity indv.Parms neighbor.Parms / state.SumDiversity  //normalize diversity
    let fNbr = state.NormalizedFit.[neighbor.Id]
    let fI = state.NormalizedFit.[indv.Id]
    let pf1I = state.P1Fit.[indv.Id]
    let stability = state.Stability.[indv.Id]
    let ({KS=ksI;Level=_},md) = indv.KS
    let ({KS=ksN;Level=_},_) = neighbor.KS
    let nKSC = 1. - state.KSCount.[ksN] //level of ks
    let coop =
        let attraction = (fNbr - fI) // |> max 0.
        let defectCoof =  if isExploitative ksI && (fI > pf1I) then IMPROVE_DEFECT_COFF else 0.
        let ksCompatibility = if isExplorative ksI && isExploitative ksN then KS_ATTRACTION_COFF else 0.
        let sameKSDefection = if ksI = ksN then -3.0 else 0.
        let kslow = nKSC ** LOW_KS_COUNT_EXPONENT
        let fitattraction = (attraction * FIT_ATTRACTION_WEIGHT)
        let diversity = d * DIVERSITY_WEIGHT
        let stability = stability * STABILITY_WEIGHT
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
    let coops = neighbors |> Seq.map (cooperation state indv) |> Seq.toList
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

let prmryKS ({KS=ks;Level=_},_) = ks

let updtScndryKS (pop:Population<IpdKS>) payout indv =
    let ksw = 
        payout 
        |> List.map (fun (idSet,f) ->  //set<id>, float [0-2]
            let nhbrId = other indv.Id idSet
            let pks = prmryKS pop.[nhbrId].KS
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

let logI st newKs indv = 
    let ps = indv.Parms |> Array.map parmToFloat
    let pksOld = prmryKS indv.KS
    let pksNew = prmryKS newKs
    {id=indv.Id; x=ps.[0]; y=ps.[1]; ks=pksNew; ksp=pksOld; gen=st.Gen; fit=indv.Fitness;} |> MIndv |> log.Post

let crtWthKS st (indv:Individual<IpdKS>) primary secondary = 
    let {KS=oldKS;Level=oldLvl} = fst indv.KS
    let ({KS=newKS;Level=_},secondary) = (primary, removePrimaryKS primary.KS secondary |> removeSituationalKS) |> promoteDomainKS
    let primary =
        if newKS = oldKS then
            {KS=newKS; Level= oldLvl * KS_ADJUST |> max MIN_INFLUENCE_LEVEL}
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
        | 0,0 -> crtWthKS st indv (fst indv.KS) Map.empty
        | 0,_ -> crtWthKS st indv (fst indv.KS) (updtScndryKS pop payout indv)
        | 1,_ ->
            let nhbr = pop.[other indv.Id (fst vmx.[0])]
            let (pks,_):IpdKS = nhbr.KS
            let (oldPks,_) = indv.KS
            if (pks.KS = oldPks.KS) then printfn "same ks %A" pks.KS
            crtWthKS st indv pks Map.empty
        | _,_ ->  
            let i = CAUtils.rnd.Value.Next(0,vmx.Length - 1)
            let nhbr = pop.[other indv.Id (fst vmx.[i])]
            let (pks,_):IpdKS = nhbr.KS
            crtWthKS st indv pks Map.empty
    let (p,_) = indv.KS
    indv

let updatePop st vmx cmprtr (pop:Population<IpdKS>) (payouts:Payout array) = 
    let pop = pop |> Array.Parallel.map(fun indv ->
        updateIndv st vmx cmprtr pop indv payouts.[indv.Id]
    )
    pop

let createState gen stability prevFitOpt (vmin,vmax) cmprtr (pop:Population<IpdKS>) =
    let nrmlzdFit = normalizePopFitness (0., 1.0) cmprtr pop
    let ksc = 
        pop 
//        |> Seq.collect(fun i-> let ({KS=ks;Level=l},m) = i.KS in (ks,l)::(Map.toList m))  
        |> Seq.map(fun i-> let pks = prmryKS i.KS in pks,1.0)  
        |> Seq.groupBy fst 
        |> Seq.map (fun (k,xs)->k, xs |>Seq.sumBy snd)
    let totalKsc = ksc |> Seq.sumBy snd
    let ksc = ksc |> Seq.map (fun (k,v) -> k, v / totalKsc) |> Map.ofSeq
    {
        SumDiversity = sampleAvgDiversity pop * float pop.Length
        NormalizedFit = nrmlzdFit
        VMin = vmin
        VMax = vmax
        P1Fit = match prevFitOpt with Some (f,_) -> f | None -> nrmlzdFit
        Stability = stability
        KSCount = ksc
        Gen = gen + 1
    }

let updateStability f = function
    | Domain,Domain -> f + 1. //|> min 5.
    | _,_           -> 0.

let rec outcome state cmprtr (pop,beliefSpace) (payouts:Payout array) =
    let vmx = (state.VMin, state.VMax)
    let pop' = updatePop state vmx cmprtr pop payouts
    let stability = state.Stability |> Array.mapi (fun i f -> updateStability f (prmryKS pop'.[i].KS, prmryKS pop.[i].KS))
    let state = createState state.Gen stability (Some (state.NormalizedFit,state.P1Fit)) vmx cmprtr pop'
    pop',
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

let game (vmin,vmax) cmprtr (pop:Population<IpdKS>) =
    let stability = Array.zeroCreate pop.Length
    let state = createState 0 stability None (vmin,vmax) cmprtr pop
    {
        Play = play state
        Payoff = payoff state
        Outcome = outcome state
    }

let ipdInfluence beliefSpace (pop:Population<IpdKS>) =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> Map.ofList
    let pop =
        pop
        |> Array.Parallel.map (fun p -> 
            let {KS=mainKS;Level=lvl},otherKs = p.KS
            let p = ksMap.[mainKS].Influence lvl p
            let p = (p,otherKs) ||> Map.fold (fun p k w -> if isExplorative k then ksMap.[k].Influence w p else p) //explorative ks go first
            let p = (p,otherKs) ||> Map.fold (fun p k w -> if isExploitative k then ksMap.[k].Influence (w*SCNRY_EXPL_KS_BOOST) p else p) //exploitative ks go last
            p)
    pop 

let knowledgeDist (vmin,vmax) comparator pop =
    let g = game (vmin,vmax)  comparator pop
    KDContinousStrategyGame.knowledgeDist comparator g

