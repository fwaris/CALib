module KDGame
open CA
open CAUtils
open FSharp.Collections.ParallelSeq

type Action = 
    | Hawk
    | Dove

type GameType = {Name : string}
type Opponents = Id[]
type PayoffMatrix = (Action * Action) -> (float*float)
type Strategy = (Action*float)[] //probability of playing action
type TypeStrategy = GameType * Strategy

type Game =
    {
        Player1  : GameType
        Player2  : GameType
        Payoff   : PayoffMatrix
    }  

let solve (g:Game)  =  [g.Player1,[| Hawk, 0.2 ; Dove, 0.8 |]; g.Player2,[| Hawk, 0.8 ; Dove, 0.2 |]]

let opponents pop network =
    pop 
    |> PSeq.collect (fun p -> network pop p.Id  |> Seq.map (fun p2 -> set[p.Id; p2.Id])) 
    |> set
    |> Set.map Set.toArray
    |> Set.toArray

let radomizePopStrategies game pop  : TypeStrategy[] = //randomy assgin a game type to each player
    let strategies = solve game
    let istrs = 
        pop 
        |> PSeq.ordered 
        |> PSeq.map (fun i -> strategies.[CAUtils.rnd.Value.Next(0,strategies.Length-1)])
        |> PSeq.toArray
    istrs


let playAction strategy = 
    //probability dist so it accumluates to 1.
    let rnd = CAUtils.rnd.Value.NextDouble()
    let rec sel (strategy:(Action*float)[]) rnd cumP i =
        let (a,p) = strategy.[i]
        let cumP = cumP + p
        if rnd >= cumP then
            sel strategy rnd cumP (i+1)
        else
            a
    sel strategy rnd 0. 0
        
let playGame game (strategies:TypeStrategy[]) i (opponents:Opponents) =
    let p1 = opponents.[0]
    let p2 = opponents.[1]
    let s1 = strategies.[p1]
    let s2 = strategies.[p2]
    let a1 = playAction (snd s1)
    let a2 = playAction (snd s2)
    let pay1,pay2 = game.Payoff (a1,a2)
    [(p1,pay1);(p2,pay2)]

let playAllGames opponents game strategies = 
    opponents 
    |> PSeq.mapi (playGame game strategies) 
    |> PSeq.collect CAUtils.yourself 
    |> PSeq.groupBy fst
    |> PSeq.map (fun (i,xs) -> i, xs |> PSeq.sumBy snd)
    |> PSeq.sortBy fst
    |> Seq.map snd
    |> Seq.toArray

let fitness (i:Individual<_>) = i.Fitness

let rec private fixedStrategyKD sign opponents game strategies (pop:Individual<_>[],beliefSpace) (network:Network<_>) =
    let targetRange = (0.1,0.9)
    //payoff
    let payoffs = playAllGames opponents game strategies
    let minP = Array.min payoffs
    let maxP = Array.max payoffs
    let scaledPayoffs = payoffs |> Array.map (scaler targetRange (minP,maxP))
    //fitness
    let minFit = pop|>Array.minBy (fun p->p.Fitness) |> fitness
    let maxFit = pop|>Array.maxBy (fun p->p.Fitness) |> fitness
    let sourceRange = if sign = 1. then (minFit,maxFit) else (-maxFit,-minFit)
    let sf f = scaler targetRange sourceRange (sign * f)
    let scaledFitness = pop |> Array.mapi (fun i p -> scaledPayoffs.[i] * sf p.Fitness)
    let pop =
        pop
        |> PSeq.ordered
        |> PSeq.map (fun p ->
            let friends = network pop p.Id
            let maxP = Seq.append friends [p] |> Seq.maxBy (fun p->scaledFitness.[p.Id])
            {p with KS=maxP.KS}
        )
        |> PSeq.toArray
    printfn "payoff %A, scaledFit %A" (minP,maxP) sourceRange
    let gtScore = scaledFitness |> PSeq.mapi (fun i f -> fst strategies.[i],f ) |> PSeq.groupBy fst |> Seq.map (fun (x,y) -> x.Name,y|>Seq.map snd|>Seq.sum)
    printfn "Winners: %A" gtScore
    let counts = strategies |> Array.countBy fst
    printfn "Types: %A" counts
    pop,beliefSpace,KD(fixedStrategyKD sign opponents game strategies)

let hawkDovePayoff = function
    | Hawk,Hawk -> 0.2, 0.2
    | Dove,Dove -> 0.5, 0.5
    | Hawk,Dove -> 0.8, 0.1
    | Dove,Hawk -> 0.1, 0.8

let hawkDoveGame = {Player1={Name="Hawk"}; Player2={Name="Dove"};Payoff=hawkDovePayoff}

let knowledgeDist minmax game pop network =
    let sign = if minmax 2. 1. then +1. else -1.
    let opponents = opponents pop network 
    let strategies = radomizePopStrategies game pop
    KD(fixedStrategyKD sign opponents game strategies)
    
