module KDDiffEvolution
open CA
open KDContinousStrategyGame
open CAUtils

type Action = unit
type Payout = unit

let play _ (indv:Individual<_>) neighbors payoff : Action = ()                    //do nothing for DE

let payoff  _ indv indvActn (nhbrActns:Action seq) : Payout =  ()                 //do nothing for DE

let influenceLevels =
    dict
        [
            Domain, 0.3
            Normative, 1.0
            Situational, 1.0
            Historical, 1.0
        ]

let il ks = match influenceLevels.TryGetValue ks with true,v -> v | _ -> 1.0

let private deInfluence beliefSpace b (pop:Population<Knowledge>) =
    let ksMap = CAUtils.flatten beliefSpace |> List.map (fun k -> k.Type, k) |> dict
    let prvGenParms = pop |> Array.map (fun ind -> Array.copy ind.Parms)
    let pop =
        pop
        |> Array.Parallel.map (fun p -> 
            let lvl = il p.KS
            let p = ksMap.[p.KS].Influence b prvGenParms pop lvl p
            p)
    pop 

let rec outcome b envCh optKind (pop,beliefSpace,_) (payouts:Payout array) =   
    let pop = deInfluence beliefSpace b pop                                      //update popluation with de influence
    pop,                                                                       
    beliefSpace,                                                               //no knowledge distribution step 
    {                                                                          //is required - all ks are DiffEvolutionKS 
        Play = play 
        Payoff = payoff 
        Outcome = outcome 
    }

let game  =
    {
        Play = play
        Payoff = payoff
        Outcome = outcome
    }

let influence () =
    let g = game
    KDContinousStrategyGame.influence g



