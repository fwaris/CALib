module KDContinousStrategyGame
open CA
open FSharp.Collections.ParallelSeq

type Neighbors = Individual array
type S<'action>  = Individual * 'action
type Payoff<'action,'payout> = Individual -> S<'action> seq -> 'payout
type Play<'action,'payout> = Individual -> Neighbors -> Payoff<'action,'payout> -> 'action
type Outcome<'action,'payout> = Individual -> 'payout -> S<'action> seq -> Individual

type private State<'action,'payout> =
    {
        Payoff  : Payoff<'action,'payout>
        Play    : Play<'action,'payout>
        Outcome : Outcome<'action,'payout>
        Sign    : float
    }

let playGame play payoff (network:Network) pop indv =
    let neighbors = network pop indv.Id
    let action = play indv neighbors payoff
    (indv,action):S<_>

let rec private csStrategy 
    state 
    (pop:Population ,beliefSpace) 
    (network:Network) 
    =
    let actions = 
        pop 
        |> PSeq.ordered
        |> PSeq.map (playGame state.Play state.Payoff network pop) 
        |> PSeq.toArray
    let pop =
        pop
        |> PSeq.ordered 
        |> PSeq.mapi (fun i indv ->
            let ns = network pop indv.Id
            let nas = ns |> Array.map (fun n ->  actions.[n.Id]) |> Seq.ofArray
            let y = state.Payoff indv  nas
            let indv = state.Outcome indv y nas
            indv)    
        |> PSeq.toArray
    pop,beliefSpace,KD(csStrategy state)

let csGameDist isBetter (pop:Individual[]) network payoff play outcome =
    let sign = if isBetter 2. 1. then +1. else -1.
    let state = 
        {
            Payoff      = payoff
            Play        = play
            Outcome     = outcome
            Sign        = sign
        }
    KD(csStrategy state)
