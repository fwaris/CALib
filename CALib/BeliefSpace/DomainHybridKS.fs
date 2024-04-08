module DomainHybridKS
///Domain knowledge source
open CA
open CAUtils
open CAEvolve
open DiffEvolutionKS
open System


///Domain default acceptance function
let rec defaultAcceptance fInfluence state envChanged (voters:Individual<_>[]) =
    let candiateBest = (Array.head voters).Fitness

    let state' =
        if state.IsBetter candiateBest state.Best then                                  //if current Domain ks type produces best 
            {state with GensSinceLastBest=0; Count=state.Count+1; Best=candiateBest}    //then keep same domain type
        else                                                                            //otherwise
            let gbs = state.GensSinceLastBest+1                                         //track gens where ks does not have best
            if gbs > 10 then                                                             
                {state with GensSinceLastBest=0; Count=state.Count+1; UseDE=not state.UseDE} //every 10th gen toggle KS
            else
                {state with GensSinceLastBest=gbs; Count=state.Count+1}                      //otherwise continue

    voters, construct state' defaultAcceptance fInfluence

///Domain default influence function
let defaultInfluence state iBest prvGenParms pop influenceLevel (ind:Individual<_>) =
    if state.UseDE then
        DiffEvolutionKS.defaultInfluence state iBest prvGenParms pop influenceLevel (ind:Individual<_>)
    else
        DomainKS.defaultInfluence state iBest prvGenParms pop influenceLevel ind

///Create Domain knowledge source
let create parmDefs optKind (fitness:Fitness) =

    let state = DiffEvolutionKS.initState parmDefs (CAUtils.comparator optKind) fitness

    DomainKS.construct state defaultAcceptance defaultInfluence

