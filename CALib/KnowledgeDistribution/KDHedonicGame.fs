module KDHedonicGame
open CA
open FSharp.Collections.ParallelSeq

type IndState = {RelFit:float; RelImprovment:float}
type GameState = {Count:int; Coalitions:Map<Id,Set<Id>>; PrevFit:float[]; Sign:float}

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

let dbg = function
    | id,(Coalition c) -> printfn "Adding %d to coalition %A" id c
    | id,(Ind i)       -> printfn  "Forming c %d,%d" id i

let joinCoalition coalitionSet (id,partner) =
    dbg (id,partner)
    match partner with
    | Coalition c  ->  Set.add (c |> Set.add id) coalitionSet
    | Ind id2      ->  Set.add (set[id;id2]) coalitionSet

let individualsInExistingCoaltions newCoalitions coalitions =
       newCoalitions 
       |> Seq.collect CAUtils.yourself
       |> Seq.choose (fun id -> if coalitions |> Map.containsKey id then Some id else None)

let findDominantKS frndsLookup dsMap id =
   let extndFrnds = frndsLookup |> Map.find id 
   let kss = extndFrnds |> Seq.collect (fun (i:Individual)->i.KS) |> Seq.countBy CAUtils.yourself |> Seq.maxBy snd
   dsMap |> Map.add id (fst kss)

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

let removeFromCoalition (pop:Individual[]) dominantKsMap (coalitions,acc) id =
    let ks = dominantKsMap |> Map.find id
    let indv = {pop.[id] with KS=set[ks]}
    let oldCoal = coalitions |> Map.find id
    printfn "removing %d from coaliton %A" id oldCoal
    let coalitions = (coalitions,oldCoal) ||> Set.fold (fun acc id -> acc |> Map.remove id) //remove all old coalition members
    let coalSansId = oldCoal |> Set.remove id
    let cs,acc =
        match Set.count coalSansId with
        | 0 -> printfn "*** unexpected emtpySet after removing coalition member"; coalitions, acc
        | 1 -> //need to disolve the old coaltion completely
            let pid = Seq.head coalSansId
            let pindv = pop.[pid]
            let pindv = {pindv with KS=pindv.KS |> Set.remove ks} 
            if pindv.KS.Count = 0 then printfn "*** ks count was zero for %d" pid
            coalitions,pindv::acc
        | x -> 
            let remanCoalMems = 
                coalSansId 
                |> Set.map (fun i->pop.[i]) 
                |> Seq.map (fun ind -> {ind with KS=ind.KS |> Set.remove ks})
                |> Seq.toList
            let acc = List.append remanCoalMems acc
            let cs = (coalitions,coalSansId) ||> Set.fold (fun acc id -> acc |> Map.add id coalSansId)
            cs,acc
    cs,indv::acc    

let isAlreadyPaired acc s = s |> Seq.exists (fun x -> acc |> Set.contains x)

let formCoalitions network pop coalitions unsatisfied =

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

    let newCoalitions = (Set.empty,globalCoalitions) ||> Seq.fold joinCoalition
    let indvsExstCoals = individualsInExistingCoaltions newCoalitions coalitions
    let frndsLookup = extendedFriends |> Map.ofSeq
    let dominantKS = (Map.empty,indvsExstCoals) ||> Seq.fold (findDominantKS frndsLookup)

    let (coalitions,indvUpdtsCoalRmvl) = 
        ((coalitions,[]),indvsExstCoals) 
        ||> Seq.fold (removeFromCoalition pop dominantKS)

    let pop = Array.copy pop            //make a copy for mutation
    for indv in indvUpdtsCoalRmvl do    //indv updates due to removal from coaltions
        pop.[indv.Id] <- indv           //first udpdate removed members to correctly updated added ones

    let coalitions,invdUpdtsCoalAdd = ((coalitions,[]),newCoalitions) ||> Set.fold (fun (acc,indvs) idst -> 
        let acc = (acc,idst) ||> Set.fold (fun acc id -> acc |> Map.add id idst)
        let coalKS = (Set.empty,idst) ||> Set.fold (fun acc id -> acc |> Set.union pop.[id].KS)
        if coalKS.Count <> idst.Count then
            printfn "ks and coalition counts do not match for %A %A" coalKS idst
        let updatedIndvs = idst |> Seq.map (fun i -> {pop.[i] with KS=coalKS}) |> Seq.toList
        acc,List.append updatedIndvs indvs
        )

    for indv in invdUpdtsCoalAdd do     //indv updates due to additon to coaltions
        pop.[indv.Id] <- indv

    validateUpdate pop coalitions

    pop,coalitions

let rec hedonicStrategy ({Count=i;Coalitions=coalitions;PrevFit=prevNormlzdFit;Sign=sign} as ksState) 
                        (pop:Individual[],beliefSpace) 
                        (network:Network) =
    let target = (0.1,0.9)
    let curNormlzdFit = normalizePopFitness target sign pop
    if i < 2 then                                                  //initially just collect enough data
        let ksState = {ksState with Count=i+1; PrevFit=curNormlzdFit}
        pop,beliefSpace,KD(hedonicStrategy ksState) 
    else
        let normlzdImprovements = normalizeImprovement target curNormlzdFit prevNormlzdFit
        let unsatisfied = calcUnsatisfied curNormlzdFit normlzdImprovements
        let pop,coalitions = formCoalitions network pop coalitions unsatisfied
        let ksState = {ksState with Count = i+1; PrevFit=curNormlzdFit; Coalitions=coalitions}
        pop,beliefSpace,KD(hedonicStrategy ksState)

let hedonicKDist isBetter (pop:Individual[]) network =
    let sign = if isBetter 2. 1. then +1. else -1.
    let state = 
        {
            Count = 0
            Coalitions = Map.empty
            PrevFit = pop |> Array.map (fun i -> i.Fitness)
            Sign = sign
        }
    KD(hedonicStrategy state)
