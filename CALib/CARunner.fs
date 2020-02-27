///Module that contains functions to run CA
//contains the step function that single steps CA
module CARunner
open CA
open FSharp.Collections.ParallelSeq

///create the belief space structure that is normally used in CAs
let defaultBeliefSpace parmDefs optKind fitness =
    Roots [ 
        Node (SituationalKS.create parmDefs optKind 15,
            [
                Leaf (HistoricalKS.create parmDefs optKind 100)
                Leaf (DomainKS.create parmDefs optKind fitness)
            ])
        Leaf (NormativeKS.create parmDefs optKind)
        Leaf (TopographicKS.create parmDefs optKind fitness)
        ]

//create belief space with Differential Evolution based 
//Domain KS. Useful for high dimensional search space where
//determining gradients across individual dimensions could be expensive
let deBeliefSpace parmDefs optKind fitness =
    Roots [ 
        Node (SituationalKS.create parmDefs optKind 15,
            [
                Leaf (HistoricalKS.create parmDefs optKind 100)
                Leaf (DiffEvolutionKS.create parmDefs optKind fitness)
            ])
        Leaf (NormativeKS.create parmDefs optKind)
        Leaf (TopographicKS.create parmDefs optKind fitness)
        ]

///evaluate the finess of the population
let evaluate fitness pop = 
    let pop =
        pop
        |> Array.Parallel.map (fun (ind:Individual<_>) -> 
            {ind with Fitness=fitness ind.Parms})
    pop

///default acceptance function used in most CAs
let acceptance topProportion mult beliefSpace (pop:Population<_>) =
    let take = (float pop.Length) * topProportion |> int
    let topInds = 
        pop 
        |> PSeq.sortBy (fun ind -> -mult * ind.Fitness) //sort in descending order so best is first
        |> Seq.truncate take
        |> Seq.toArray
    topInds

///default belief space update function
let update envChanged beliefSpace bestInds  =          
    let rec update bestInds ksTree  =
        match ksTree with
        | Roots ksList                  ->  Roots (ksList |> List.map (update bestInds))
        | Node ({Accept=accept},ksList) ->
                                            let inds,ks = accept envChanged bestInds
                                            Node (ks,ksList |> List.map (update inds))
        | Leaf {Accept=accept}          -> 
                                            let _,ks = accept envChanged bestInds
                                            Leaf ks
    update bestInds beliefSpace


///single step CA
let step envChanged (st:TimeStep<_>)  maxBest =
    let ca = st.CA
    let c  = st.Count
    let p  = st.Progress
    let ec = st.EnvChngCount
    let best = st.Best
    let mult = CAUtils.mult ca.Optimization
    let isBetter = CAUtils.comparator ca.Optimization
    let pop             = evaluate ca.Fitness.Value ca.Population
    let topInds         = ca.Acceptance ca.BeliefSpace pop
    let oldBest = 
        if envChanged then 
            match best with 
            | []   -> [] 
            | x::_ -> 
                let f = ca.Fitness.Value x.MParms //re-evaluate old best under new fitness landscape
                [{x with MFitness=f}]
        else 
            best
    let ec = if envChanged then ec + 1 else ec

    let reactToEnvChange = 
        match ca.EnvChngSensitivity with 
        | Insensintive when envChanged              -> Track
        | Insensintive                              -> NoChange
        | Every m when envChanged && (ec % m) = 0   -> Adjust 
        | Every _ when envChanged                   -> Track
        | Every _                                   -> NoChange

    let newBest = 
        match oldBest,topInds with
        | _ ,[||]   -> oldBest
        | [],is     -> [CAUtils.toMarker is.[0]]
        | b::_,is when (mult*is.[0].Fitness) > (mult*b.MFitness) -> (CAUtils.toMarker is.[0]::oldBest) |> List.truncate maxBest
        | _         -> oldBest

    let newBest = 
        CAUtils.Incidental.compare st.IBest isBetter newBest.[0]
        |> Option.map (fun m -> (m::newBest) |> List.truncate maxBest)
        |>Option.defaultValue newBest

    let blSpc           = ca.Update reactToEnvChange ca.BeliefSpace topInds
    let fInfluence      = match ca.Influence with Influence(k) -> k
    let pop,blSpc,finf = fInfluence st.IBest reactToEnvChange pop blSpc ca.Network ca.Fitness ca.Optimization 
    //let pop             = ca.Influence blSpc pop
    {
        CA =
            {ca with
                Population   = pop
                BeliefSpace  = blSpc
                Influence    = finf
            }
        Best = newBest
        Progress = newBest.[0].MFitness::p |> List.truncate 100
        Count = c + 1
        EnvChngCount = ec
        IBest = st.IBest
    }

let DEFAULT_ENV_CHANGE_EPSILON = 0.000001

//step function for dynamic environments
let dynStep envChangeEpsilon timeStep maxBest = 
    match timeStep.Best with
    | [] -> step false timeStep maxBest
    | x::_->
        let prevFit = x.MFitness
        let newFit = timeStep.CA.Fitness.Value x.MParms
        let envCh =  abs(newFit - prevFit) > envChangeEpsilon
        step envCh timeStep maxBest

///run till termination
let run desc termination maxBest ca =
    let rec loop stp = 
        let stp = step false stp maxBest
        let best = if stp.Best.Length > 0 then stp.Best.[0].MFitness else 0.0
        match stp.Progress.Length with
        | 0 -> printfn "starting '%s'" desc
        | 1 -> printfn "'%s' %d %f %A" desc stp.Count stp.Progress.[0] stp.Best.[0].MParms
        | _ when stp.Progress.[0] = stp.Progress.[1] |> not -> 
            printfn "'%s' %d %f %A" desc stp.Count stp.Progress.[0] stp.Best.[0].MParms
        | _ -> ()
        if termination stp then
            stp
        else
            loop stp
    loop (CAUtils.initStep ca)


let ``terminate if no improvement in 5 generations`` (step:TimeStep<_>) =
    match step.Progress with
    | f1::f2::f3::f4::f5::_ when f1=f2 && f2=f3 && f3=f4 && f4=f5 -> true
    | _ -> false

