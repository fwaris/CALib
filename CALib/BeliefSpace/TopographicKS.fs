module TopographicKS

open CA
open CAUtils
open CAEvolve
open MachineLearning

let eSigma = 3.0

type Centroid =
    {
        Center  : float[]
        Count   : int
        Best    : Parm[]
        BestFit : float
    }

type State<'a> = 
    {
        Centroids       : Centroid list
        Individuals     : Individual<'a> array
        ToParms         : float[] -> Parm[]
        Fitness         : Parm[] -> float
        FitScaler       : float
        SpinWheel       : (Centroid*float)[]
    }

let MAX_INDVS = 1000
let cfact xs k =  KMeansClustering.randomCentroids Probability.RNG.Value xs k |> List.map (fun (x:float[])->x,[])
let cdist (x,_) y = KMeansClustering.euclidean x y
let cavg (c,_) xs = (KMeansClustering.avgCentroid c xs),xs

let toCentroid state (c,members) =
    let lbest = members |> Seq.map (fun x -> state.ToParms x) |> Seq.maxBy (fun ps -> (state.Fitness ps) * state.FitScaler)
    {
        Center = c
        Count  = Seq.length members
        Best = lbest
        BestFit = state.Fitness lbest
    }

let updateClusters state voters =
    let vns = 
        Seq.append state.Individuals voters 
        |> Seq.sortByDescending (fun i-> state.FitScaler * i.Fitness) 
        |> Seq.truncate MAX_INDVS 
        |> Seq.toArray
    let parmsArray = vns |> Array.map(fun i->i.Parms |> Array.map parmToFloat)
    // type CentroidsFactory<'a> = 'a seq -> int -> Centroid<'a> seq
    let k = match vns.Length with x when x < 10 -> 2 | x when x < 20 -> 4 | x when x < 100 -> 5 | x when x < 500 -> 7 | _ -> 10
    let kcntrods,_ = KMeansClustering.kmeans cdist cfact cavg  parmsArray k
    let cntrds = kcntrods |> Seq.filter (fun (_,ls)->List.isEmpty ls |> not) |> Seq.map (toCentroid state) |> Seq.toList
    let _,wheel = cntrds |> Seq.map (fun c->c,float c.Count) |> Seq.toArray |> Probability.createWheel
    { state with Centroids = cntrds; SpinWheel=wheel}

let influenceIndv state temp (indv:Individual<_>) =
    let cntrd = Probability.spinWheel state.SpinWheel 
    let p2 = cntrd.Best |> Array.map (evolveS temp eSigma)
    {indv with Parms=p2}

let toParm (p,f) = 
    match p with
    | F(_,mn,mx)  -> F(f,mn,mx)
    | F32(_,mn,mx)-> F32(float32 f,mn,mx)
    | I(_,mn,mx)  -> I(int f,mn,mx)
    | I64(_,mn,mx)-> I64(int64 f,mn,mx)

let initialState parms isBetter fitness =
    let toParms point = Seq.zip parms point |> Seq.map toParm |> Seq.toArray
    {
        Centroids = []
        Individuals = [||]
        ToParms     = toParms
        Fitness     = fitness
        FitScaler   = if isBetter 1. 0. then 1. else -1.
        SpinWheel   = [||]
    }
    
let create parms isBetter fitness =
    let create state fAccept : KnowledgeSource<_> =
        {
            Type        = Topgraphical
            Accept      = fAccept state
            Influence   = influenceIndv state
        }

    let rec acceptance state (voters:Individual<_> array) =
        let state = updateClusters state voters
        voters,create state acceptance 
           
    let state = initialState parms isBetter fitness
    create state acceptance
