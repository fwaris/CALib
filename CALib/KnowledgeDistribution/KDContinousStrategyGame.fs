module KDContinousStrategyGame
// abstract definitions of Play, Payoff & Outcome functions
// for game based Knowledge Distribution
open CA

type Neighbors<'k> = Individual<'k> array

type Payoff<'action,'payout,'k> = 
    OptimizationKind 
        -> Individual<'k>
        -> 'action 
        -> 'action seq 
        -> 'payout

type Play<'action,'payout,'k> = 
    OptimizationKind 
        -> Individual<'k> 
        -> Neighbors<'k> 
        -> Payoff<'action,'payout,'k> 
        -> 'action

type Outcome<'action,'payout,'k> = 
    EnvChngeType
        -> OptimizationKind
        -> Population<'k> * BeliefSpace<'k> * Network<'k>
        -> 'payout array 
        -> Population<'k> * BeliefSpace<'k> * CSGame<'action,'payout,'k>

and CSGame<'action,'payout,'k> =
    {
        Play        : Play<'action,'payout,'k>
        Payoff      : Payoff<'action,'payout,'k>
        Outcome     : Outcome<'action,'payout,'k>
    }

let playGame cmprtr play payoff (network:Network<_>) pop indv =
    let neighbors = network pop indv.Id
    let action = play cmprtr indv neighbors payoff
    action

let rec private csStrategy 
    game 
    envCh
    pop
    beliefSpace
    network
    fitness
    cmprtr
    =
    let actions = 
        pop 
        |> Array.Parallel.map (playGame cmprtr game.Play game.Payoff network pop) 

    let payoffs =
        pop
        |> Array.Parallel.map (fun indv ->
            let indvActn = actions.[indv.Id]
            let nhbrs = network pop indv.Id
            let nhbrActns = 
                nhbrs 
                |> Array.map (fun n ->  actions.[n.Id]) 
                |> Seq.ofArray
            game.Payoff cmprtr indv indvActn  nhbrActns)

    let pop,beliefSpace,game = game.Outcome envCh cmprtr (pop,beliefSpace,network) payoffs
    pop,beliefSpace,Influence(csStrategy game)

let influence gameConfig =
    Influence(csStrategy gameConfig)
