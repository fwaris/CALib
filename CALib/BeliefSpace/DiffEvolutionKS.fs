﻿///Alternate Domain knowledge source implementation
///based on differential evolution method
//Standard Domain KS may be slow in high-dimensional problem spaces so this can be used instead.
module DiffEvolutionKS
open CA
open CAUtils
open CAEvolve
open Probability

type DEState = {ParmDefs:Parm[]; IsBetter:Comparator; Fitness:Fitness; Best:float; Count:int; GensSinceLastBest:int; UseDE:bool}

let CR = 0.9
let F = 0.8

let rec newRnd m oldRands = 
    let r = RNG.Value.Next(m)
    if Set.contains r oldRands then newRnd m oldRands else r

let rec rand3 tgtIndx m = 
    if m < 4 then failwith "at least 4 parents required"
    let rndVec,_ = (([],set[tgtIndx]),[0;1;2]) ||> List.fold(fun (rs,oldRnds) _ -> let r = newRnd m oldRnds in (r::rs,Set.add r oldRnds))
    rndVec |> Seq.toArray

let applyDE state (prvGenParms:PrevGenParms) (pop:_[]) (targetInv:Individual<_>) =
    let yab = rand3 targetInv.Id pop.Length
    let y = prvGenParms.[yab.[0]]
    let a = prvGenParms.[yab.[1]]
    let b = prvGenParms.[yab.[2]]
    let iParms = targetInv.Parms |> Array.copy
    state.ParmDefs |> Array.iteri (fun i pd ->
        let r = RNG.Value.Next(0.,1.)
        let pVal = 
            if r <= CR then 
                y.[i] + F * (a.[i] - b.[i])
            else
                y.[i]
        iParms.[i] <- clampP pVal pd
    )
    iParms


let initState parmDefs isBetter fitness = 
    {
        ParmDefs=parmDefs
        IsBetter=isBetter
        Fitness=fitness
        Count=0
        Best = 0.
        GensSinceLastBest = 0
        UseDE=true
    }

let construct state fAccept fInfluence : KnowledgeSource<_> =
    {
        Type        = Domain
        Accept      = fAccept fInfluence state
        Influence   = fInfluence state
    }

let rec defaultAcceptance fInfluence state envChanged voters = voters, construct state defaultAcceptance fInfluence

let defaultInfluence state iBest prvGenParms pop influenceLevel (ind:Individual<_>) =
    let oldFit = ind.Fitness
    let newParms = applyDE state prvGenParms pop ind
    let newFit = state.Fitness.Value newParms
    if state.IsBetter newFit oldFit then
        CAUtils.Incidental.update iBest state.IsBetter newFit newParms
        newParms |> Array.iteri (fun i p -> ind.Parms.[i] <- p) //mutation
        {ind with Fitness = newFit; IsStale=false} //since indvidual was re-evaluated in this intermediate step 
    else                                           //no need to re-evaluate it in the next round so mark as not stale
        ind

let create parmDefs optKind (fitness:Fitness) =

    let state = initState parmDefs (CAUtils.comparator optKind) fitness

    construct state defaultAcceptance defaultInfluence
