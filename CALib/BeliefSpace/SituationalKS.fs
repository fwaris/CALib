module SituationalKS
open CA
open CAUtils
open CAEvolve

let eSigma = 3.0

type State<'a> = 
    {
        Exemplars    :   Individual<'a> array
        SpinWheel    :   (int*float)array
    }

let fitweights isBetter (exemplars:Individual<_> array) =
    let tx m = if isBetter 2. 1. then m else 1.0 / m
    let wghts = exemplars |> Array.map (fun i-> tx i.Fitness)
    wghts 

let parmDiversity p1 p2 = 
    (p1,p2)
    ||> Array.map2 (fun a b -> parmDiff a b |> parmToFloat |> abs) 
    |> Array.sum

let makeBins (mn:float) mx bins = 
    if mn > mx then failwith "mn > mx"
    let r = mx - mn
    let bw = r / bins
    (mn,mn+bw,1) |> Seq.unfold (fun (mn,mx,c) -> 
        if c > int bins then
            None
        else
            Some ((mn,mx), (mx,mx+bw,c+1)))
    |> Seq.toList

// makeBins 1. 17. 5.
// makeBins 1. 1.03 5.
// makeBins -10. -2.5 5.

//round robbin collect elements in the bins
//collect 1st element from 1st bin, 1st element from 2nd bin until no more bins
//then repeat starting from the 1st bin and 2nd element...
let rec clct acc l1 l2 =
    match l1, l2 with 
    | [],[]             -> acc |> List.rev
    | [],_              -> clct acc (List.rev l2) []
    | (_,[])::rest,_    -> clct acc rest l2
    | (b,x::r1)::rest,_ -> clct (x::acc) rest ((b,r1)::l2)

// clct [] ['a',[1;2]; 'b',[3;4]; 'c',[5;6]] []


let pickExamplars isBetter prevE voters =
    let tx = if isBetter 2. 1. then -1. else 1.
    let ex = Seq.append prevE  voters |> Seq.toArray
    let ex = Array.sortBy (fun (x:Individual<_>) -> tx * x.Fitness) ex
    let best = ex.[0]
    let ex = ex.[1..] //all others, i.e. not best
    let fitVals = ex |> Array.map (fun x->tx * x.Fitness)
    let mnFit = fitVals |> Array.min
    let mxFit = fitVals |> Array.max
    let bins = makeBins mnFit (mxFit + 0.0001) 5.
    let binned = (Map.empty,ex) ||> Array.fold (fun acc indv -> 
        let f1 = tx * indv.Fitness
        let b = bins |> List.find (fun (mn,mx) -> mn <= f1 && f1 < mx)
        match acc |> Map.tryFind b with
        | Some ls -> indv::ls
        | None    -> [indv]
        |> fun x -> acc |> Map.add b x
        )
    let binned = //sort each bin by decreasing order of divesity from best
        binned 
        |> Map.map (fun k v -> v |> List.sortBy (fun i-> -1. * (parmDiversity best.Parms i.Parms)))
        |> Map.toSeq
        |> Seq.sortBy (fun ((mn,mx),_) -> mx) //sort all bins by decreasing fitness
        |> Seq.toList
    best::(clct [] binned [])    

let create isBetter maxExemplars =
    let create state fAccept fInfluence : KnowledgeSource<_> =
        {
            Type        = Situational
            Accept      = fAccept fInfluence state
            Influence   = fInfluence state
        }

    let rec acceptance 
        fInfluence 
        state
        (voters : Individual<_> array) =
        let explrs = pickExamplars isBetter state.Exemplars voters |> List.truncate maxExemplars |> List.toArray
//        for e in explrs do printf "%0.2f [%A]" e.Fitness (parmToFloat e.Parms.[0] ,parmToFloat e.Parms.[1] )
//        printfn ""
        let weights = fitweights isBetter explrs
        let wheel = Probability.createWheel weights
        let state = {Exemplars=explrs; SpinWheel=wheel}
        voters, create state acceptance fInfluence
    
    let influence state s (ind:Individual<_>) =
        match state.Exemplars with
        | [||] -> ind
        | x -> 
            let i = Probability.spinWheel state.SpinWheel
            let choosen = x.[i]
//            printfn "sit i = %d %A" i choosen
            {ind with Parms=choosen.Parms |> Array.map (evolveS s eSigma)}      
    create {Exemplars=[||];SpinWheel=[||]} acceptance influence
