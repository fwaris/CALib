module KDContinousStrategyGame
open CA
open FSharp.Collections.ParallelSeq

type Neighbors = Individual array

type Payoff<'action,'payout> = 
    Comparator 
        -> Individual 
        -> 'action 
        -> 'action seq 
        -> 'payout

type Play<'action,'payout> = 
    Comparator 
        -> Individual 
        -> Neighbors 
        -> Payoff<'action,'payout> 
        -> 'action

type Outcome<'action,'payout> = 
    Comparator
        -> Population * BeliefSpace 
        -> 'payout array 
        ->  Population * BeliefSpace * CSGame<'action,'payout>

and CSGame<'action,'payout> =
    {
        Play        : Play<'action,'payout>
        Payoff      : Payoff<'action,'payout>
        Outcome     : Outcome<'action,'payout>
    }

let playGame cmprtr play payoff (network:Network) pop indv =
    let neighbors = network pop indv.Id
    let action = play cmprtr indv neighbors payoff
    action

let rec private csStrategy 
    cmprtr
    game 
    (pop,beliefSpace)
    (network:Network) 
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
