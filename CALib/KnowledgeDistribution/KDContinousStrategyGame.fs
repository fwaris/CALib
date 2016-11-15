module KDContinousStrategyGame
open CA
open FSharp.Collections.ParallelSeq

type Neighbors<'k> = Individual<'k> array

type Payoff<'action,'payout,'k> = 
    Comparator 
        -> Individual<'k>
        -> 'action 
        -> 'action seq 
        -> 'payout

type Play<'action,'payout,'k> = 
    Comparator 
        -> Individual<'k> 
        -> Neighbors<'k> 
        -> Payoff<'action,'payout,'k> 
        -> 'action

type Outcome<'action,'payout,'k> = 
    Comparator
        -> Population<'k> * BeliefSpace<'k> 
        -> 'payout array 
        ->  Population<'k> * BeliefSpace<'k> * CSGame<'action,'payout,'k>

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
    cmprtr
    game 
    (pop,beliefSpace)
    (network:Network<_>) 
    =
    let actions = 
        pop 
        |> PSeq.ordered
        |> PSeq.map (playGame cmprtr game.Play game.Payoff network pop) 
        |> PSeq.toArray
    let payoffs =
        pop
        |> PSeq.ordered 
        |> PSeq.map (fun indv ->
            let indvActn = actions.[indv.Id]
            let nhbrs = network pop indv.Id
            let nhbrActns = 
                nhbrs 
                |> Array.map (fun n ->  actions.[n.Id]) 
                |> Seq.ofArray
            game.Payoff cmprtr indv indvActn  nhbrActns)
        |> PSeq.toArray
    let pop,beliefSpace,game = game.Outcome cmprtr (pop,beliefSpace) payoffs
    pop,beliefSpace,KD(csStrategy cmprtr game)

let knowledgeDist comparator gameConfig =
    KD(csStrategy comparator gameConfig)
