module KDHedonicGame
open CA
open FSharp.Collections.ParallelSeq

type IndState = {RelFit:float; RelImprovment:float}
type GameState = {Count:int; Coalitions:Map<Id,Set<Id>>; PrevFit:float[]; Sign:float; KSset:Set<Knowledge>}

let normalizePopFitness target sign (pop:Individual[]) =
    let currentFit = pop |> PSeq.ordered |> PSeq.map (fun p -> p.Fitness * sign) //converts minimization to maximization (higher fitness is better)
    let minFit = currentFit |> PSeq.min
    let maxFit = currentFit |> PSeq.max
    let scaler = CAUtils.scaler target (minFit,maxFit) 
    currentFit |> PSeq.map scaler |> PSeq.toArray //scale fitness to target range

let normalizeImprovement target curNormlzdFit prevNormlzdFit =
    let improvements = Array.map2 (fun a b -> a - b) curNormlzdFit prevNormlzdFit
    let minImp = Array.min improvements
    let maxImp = Array.max improvements
    let scaler = CAUtils.scaler target (minImp,maxImp) 
    Array.map scaler improvements

let calcUnsatisfied curNormlzdFit normlzdImprovements =
    Array.map2 (fun a b -> a * b) curNormlzdFit normlzdImprovements 
    |> Array.mapi (fun i f->i,f)
    |> Array.sortBy snd
    |> Array.take (curNormlzdFit.Length/10) //take top 10% as 'unsatisfied'
    |> Array.map fst

let parmDiff p1 p2 = abs (CAUtils.parmToFloat p1 - CAUtils.parmToFloat p2) //p1 norm

let mutualUtility (p1:Individual) (p2:Individual)  =
    if p1.KS |> Set.intersect p2.KS |> Set.isEmpty |> not then 
        -99999999.0
    else
        Array.map2 parmDiff p1.Parms p2.Parms  |> Seq.sum 

let mutualUtilitiyCoalition (pop:Individual[]) (p1:Individual) (coalition:Set<Id>) =
    let ksSet = (Set.empty,coalition) ||> Set.fold (fun acc id -> acc |> Set.union pop.[id].KS)
    if p1.KS |> Set.intersect ksSet |> Set.isEmpty |> not  then
        -9999999.0
    else
        coalition 
        |> Seq.map (fun i -> pop.[i])
        |> Seq.map (mutualUtility p1)
        |> Seq.sum

type Partner = Ind of Id | Coalition of Set<Id>

let mutualUtilityPartner pop indv prtnr =
    match prtnr with
    | Coalition c   -> prtnr, mutualUtilitiyCoalition pop indv c
    | Ind id        -> prtnr, mutualUtility indv pop.[id]

let extendFriends (pop:Individual array) coalitions (friends:Individual array) =
    let initialFriends = friends |> Array.map (fun i -> i.Id) |> set
    (initialFriends,friends) ||> Array.fold (fun acc i -> 
        match coalitions |> Map.tryFind i.Id with
        | Some coalitionMembers -> Set.union coalitionMembers acc
        | None                  -> acc)
    |> Set.map (fun i -> pop.[i])
    |> Set.toArray

let removeExistingCoalitionPartners id coalitions extendedFriends =
    match coalitions |> Map.tryFind id with
    | Some s -> extendedFriends |> Array.filter (fun i -> s |> Set.contains i.Id |> not)
    | None   -> extendedFriends

let findPartners (pop:Individual[]) coalitions (id,extendedFriends:Individual[]) = 
    let extendedFriends = removeExistingCoalitionPartners id coalitions extendedFriends
    let possiblePartners = (Set.empty,extendedFriends) ||> Array.fold (fun acc i -> 
        match coalitions |> Map.tryFind i.Id with 
        | Some s -> Set.add (Coalition s) acc
        | None   -> Set.add (Ind i.Id) acc)
    let possiblePartners = possiblePartners |> Set.remove (Ind id) //exclude self from partner list
    let seekingIndv = pop.[id]
    possiblePartners 
    |> Seq.map (mutualUtilityPartner pop seekingIndv)
    |> Seq.sortBy (fun (_,v) -> -v)
    |> Seq.filter (fun (_,v) -> v > 0.)
    |> Seq.map (fun p -> id,p)
    |> Seq.toArray

let dbg (pop:Individual[]) = function
    | id,(Coalition c) -> printfn "Adding %d (%A) to coalition %A (%A)" id pop.[id].KS c (pop.[Set.minElement c].KS)
    | id,(Ind i)       -> printfn  "Forming c %d (%A), %d (%A" id pop.[id].KS i pop.[i].KS

let joinCoalition pop coalitionSet (id,partner) =
    dbg pop (id,partner)
    match partner with
    | Coalition c  ->  Set.add (c |> Set.add id) coalitionSet
    | Ind id2      ->  Set.add (set[id;id2]) coalitionSet

let findDominantKS (coalKSset:Set<Knowledge>) nhbrKSStrengths id =
    let ordered = nhbrKSStrengths |> Map.find id
    let filtered = ordered |> Array.filter (fun (ks,w) -> coalKSset.Contains ks)
    if filtered.Length =  0 then
        failwithf "empty ks set for indv %d, ordered %A" id ordered
    fst filtered.[0]

let validateUpdate pop (coalitions:Map<Id,Set<Id>>) =
    pop |> Array.iter (fun i -> 
        let ksSet = i.KS
        match ksSet.Count with
        | 0 -> printfn "*** empty ks set for id %d" i.Id
        | 1 -> if coalitions |> Map.containsKey i.Id then printfn "*** indv %d with 1 KS is part of coaltion" i.Id
        | x -> 
            let c = coalitions |> Map.tryFind i.Id
            match c with 
            | Some s -> if s.Count <> ksSet.Count then printfn "*** KS count and coalition member count differs for id %d" i.Id
            | None -> printfn "***  indv with multiple KS not in coalitions %d" i.Id
        )
    coalitions |> Map.iter (fun k s -> 
        let s2 = (Set.empty,s) ||> Set.fold (fun acc id -> acc |> Set.add (coalitions |> Map.find id))
        if s2.Count > 1 then 
            printfn "*** member was found in multiple coaltions %A" s2
        )

let assignKSToLeavingIndvs coalKSset (curNormlzdFit:float[]) dominantKS newCoalition =
    let leavingIndvs = newCoalition |> Set.filter (fun id -> dominantKS |> Map.containsKey id)
    let fitstLeavIndv = leavingIndvs |> Seq.maxBy (fun i -> curNormlzdFit.[i])
    let domKS = dominantKS |> Map.find fitstLeavIndv
    let rmngLeavIndvs = leavingIndvs |> Set.remove fitstLeavIndv
    let _,leavingKSs = 
        ((coalKSset |> Set.remove domKS,Map.empty),rmngLeavIndvs) 
        ||> Set.fold (fun (ksSet,acc) id -> 
            let ks = Set.minElement ksSet
            let ksSet = ksSet.Remove ks
            ksSet,acc |> Map.add id ks
        )
    leavingKSs |> Map.add fitstLeavIndv domKS

let breakCoalition 
    (pop:Individual[])       //this will be mutated
    (curNormlzdFit:float[]) 
    nhbrKSStrengths 
    coalitions
    ((coalition:Set<Id>),(leavingIndvs:Set<Id>))
    = 
    let ftstIndv = leavingIndvs |> Seq.maxBy (fun id -> curNormlzdFit.[id]) //fittest leaving individual
    let coalKSset = pop.[ftstIndv].KS
    if coalKSset.Count <> coalition.Count then failwithf "KS count mismatch %A %A" coalition coalKSset
    let dominantKS = findDominantKS coalKSset nhbrKSStrengths ftstIndv
    let remainKS =  Set.remove dominantKS coalKSset
    let remainLeavIndvs = Set.remove ftstIndv leavingIndvs

    let remainKS, remainLeavKSAssignments = 
        ((remainKS,Map.empty),remainLeavIndvs) 
        ||> Set.fold (fun (accKS,accIndv) id ->
            let ks = Set.minElement accKS
            let accKS = Set.remove ks accKS
            let accIndv = Map.add id ks accIndv
            accKS,accIndv)

    let leaveKSAssingments = remainLeavKSAssignments |> Map.add  ftstIndv dominantKS
    printfn "breaking %A (%A) for leaving %A (%A)" coalition coalKSset leavingIndvs leaveKSAssingments

    let remainingCoalition = (coalition,leavingIndvs) ||> Set.fold (fun acc k -> Set.remove k acc)
    let remainingMembers = remainingCoalition |> Seq.map (fun id -> {pop.[id] with KS=remainKS}) |> Seq.toList
    let leavingMembers = leaveKSAssingments |> Map.toSeq |> Seq.map (fun (id,ks) -> {pop.[id] with KS=set[ks]}) |> Seq.toList

    let coalitions = (coalitions,coalition) ||> Set.fold (fun acc id -> acc |> Map.remove id) //update coalition map

    let coalitions = 
        if remainingCoalition.Count > 1 then
            (coalitions,remainingCoalition) ||> Set.fold (fun acc id -> acc |> Map.add id remainingCoalition)
        else
            coalitions

    for indv in leavingMembers do
        pop.[indv.Id] <- indv

    for indv in remainingMembers do
        pop.[indv.Id] <- indv

    coalitions

let processLeavingMembers 
    (pop:Individual[])        //this will be mutated
    (curNormlzdFit:float[]) 
    frndsLookup 
    nhbrKSStrengths
    coalitions
    (newCoalition:Set<Id>) =

    let leavingMembers = newCoalition |> Seq.filter (fun i -> frndsLookup |> Map.containsKey i)

    let lvngMemByCoal = 
        leavingMembers 
        |> Seq.choose (fun id -> coalitions |> Map.tryFind id |> Option.map (fun cs -> cs,id))
        |> Seq.groupBy fst
        |> Seq.map (fun (cs,ids) -> cs,ids |> Seq.map snd |> set)

    (coalitions,lvngMemByCoal) ||> Seq.fold (breakCoalition pop curNormlzdFit nhbrKSStrengths)
   

let processNewMembers 
    availableKsSet
    (pop:Individual[])   //this will be mutated; assumes pop reflects changes do to breaking coalitions
    coalitions 
    newCoalition =
    let coalitions = (coalitions,newCoalition) ||> Set.fold (fun acc id -> acc |> Map.add id newCoalition)
    let coalKS = (Set.empty,newCoalition) ||> Set.fold (fun acc id -> acc |> Set.union pop.[id].KS)

    let coalKS =
        if coalKS.Count < newCoalition.Count then
            //need to add a new ks as the process of breaking and joining changed final set for this coalition
            //choose at random from the available
            let possible = Set.difference availableKsSet coalKS |> Set.toArray
            let ks = possible.[CAUtils.rnd.Value.Next(0,possible.Length-1)]
            Set.add ks coalKS
        elif coalKS.Count > newCoalition.Count then
            failwithf "ks and coalition counts do not match for %A %A" coalKS newCoalition
        else
            coalKS

    newCoalition |> Seq.iter (fun i -> pop.[i] <- {pop.[i] with KS=coalKS})
    coalitions
        
let isAlreadyPaired acc s = s |> Seq.exists (fun x -> acc |> Set.contains x)

let formCoalitions availableKS network pop coalitions curNormlzdFit unsatisfied =

    let extendedFriends = 
        unsatisfied 
        |> Seq.map (fun id -> id, network pop id)
        |> Seq.map (fun (id,friends) -> id, extendFriends pop coalitions friends)
        |> Seq.cache

    let localCoalitions =
        extendedFriends
        |> Seq.collect (findPartners pop coalitions)
        |> Seq.sortBy (fun (id,(p,v)) -> -v)
        |> Seq.map (fun (id,(p,_)) -> id,p)
        |> Seq.cache

    let _,globalCoalitions = 
        ((Set.empty,[]),localCoalitions) 
        ||> Seq.fold(fun ((accId,accPrtnr) as acc) ((id,prtnr) as p) -> 
        if accId |> Set.contains id then
            acc //skip if indv is already in a coalition
        else
            match prtnr with 
            | Coalition s -> if isAlreadyPaired accId s then 
                                acc 
                             else 
                                Set.add id accId |> Set.union s, p::accPrtnr
            | Ind i       -> if Set.contains i accId then
                                acc
                             else
                                Set.add id accId |> Set.add i, p::accPrtnr
        )

    let newCoalitions = (Set.empty,globalCoalitions) ||> Seq.fold (joinCoalition pop)
    let frndsLookup = Map.ofSeq extendedFriends

    let nhbrKSStrengths = 
        extendedFriends 
        |> Seq.map (fun (id,indvs) -> 
            id, 
            indvs 
            |> Seq.collect (fun i->i.KS |> Set.union pop.[id].KS)
            |> Seq.countBy CAUtils.yourself
            |> Seq.sortBy (fun (_,w) -> -w)
            |> Seq.toArray
            )
        |> Map.ofSeq

    let pop = Array.copy pop            //make a copy for mutation

    let coalitions = 
        (coalitions,newCoalitions) 
        ||> Seq.fold (processLeavingMembers pop curNormlzdFit frndsLookup nhbrKSStrengths)

    let coalitions = (coalitions,newCoalitions) ||> Seq.fold (processNewMembers availableKS pop)

    validateUpdate pop coalitions

    pop,coalitions

let rec private hedonicStrategy 
    ({Count=i;Coalitions=coalitions;PrevFit=prevNormlzdFit;Sign=sign} as ksState) 
    (pop:Individual[],beliefSpace) 
    (network:Network) 
    =
    let target = (0.1,0.9)
    let curNormlzdFit = normalizePopFitness target sign pop
    if i < 2 then                                                  //initially just collect enough data
        let ksState = {ksState with Count=i+1; PrevFit=curNormlzdFit}
        pop,beliefSpace,KD(hedonicStrategy ksState) 
    else
        let normlzdImprovements = normalizeImprovement target curNormlzdFit prevNormlzdFit
        let unsatisfied = calcUnsatisfied curNormlzdFit normlzdImprovements
        let pop,coalitions = formCoalitions ksState.KSset network pop coalitions curNormlzdFit unsatisfied
        let ksState = {ksState with Count = i+1; PrevFit=curNormlzdFit; Coalitions=coalitions}
        pop,beliefSpace,KD(hedonicStrategy ksState)

let knowledgeDist isBetter (pop:Individual[]) network =
    let sign = if isBetter 2. 1. then +1. else -1.
    let state = 
        {
            Count = 0
            Coalitions = Map.empty
            PrevFit = pop |> Array.map (fun i -> i.Fitness)
            Sign = sign
            KSset = (Set.empty,pop) ||> Array.fold (fun acc p -> acc |> Set.union p.KS)
        }
    KD(hedonicStrategy state)
