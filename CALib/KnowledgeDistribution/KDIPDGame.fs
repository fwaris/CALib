module KDIPDGame
open CA
open KDContinousStrategyGame
open FSharp.Collections.ParallelSeq

type W = Set<Id> * float
type Action = W list
type Payout = Action

let cooperation  neighbor indv =  set[indv.Id; neighbor.Id], 0.1 //tbd

let relativeCoop totalCoop (s,coop) = s,coop / totalCoop

let play _ indv neighbors payoff : Action =
    let coops = neighbors |> Seq.map (cooperation indv) |> Seq.toList
    let sumCoops = coops |> List.sumBy (fun (_,c) -> c)
    let ws = coops |> List.map (relativeCoop sumCoops)
    ws

let payoff _ indv indvActn (nhbrActns:Action seq) : Payout =
    let m1 = indvActn |> Map.ofList
    let payoff = 
        nhbrActns 
        |> Seq.choose (fun acts -> 
            acts 
            |> List.tryPick (fun (s,c1) ->
                m1 
                |> Map.tryFind s
                |> Option.map (fun c2 -> s, c1+c2))
            )
        |> Seq.toList
    if payoff.Length <> (Seq.length nhbrActns) then
        failwithf "neighbor action counts not matched %A %A" indv nhbrActns
    payoff

let updatePop cmprtr pop (payouts:Payout array) = pop
    

let rec outcome cmprtr (pop,beliefSpace) (payouts:Payout array) =
    let pop = updatePop cmprtr pop payouts
    pop,
    beliefSpace,
    {
        Play = play
        Payoff = payoff
        Outcome = outcome
    }

let init() =
    {
        Play = play
        Payoff = payoff
        Outcome = outcome
    }
