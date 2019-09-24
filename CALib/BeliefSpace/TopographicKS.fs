module TopographicKS
//this version of TopographicalKS is based on Brainstorm Optimization method (BSO)

//*** note check update of CIndvs in state

open CA
open CAUtils
open CAEvolve
open MachineLearning

let eSigma = 1.0

type Centroid =
    {
        Center  : float[]
        Count   : int
        Best    : float[]
        BestFit : float
    }

type CIndv = {CParms:float[]; CFitness:float}

type State = 
    {
        IsBetter        : Comparator
        Centroids       : Centroid list
        CIndvs          : Marker array
        Fitness         : Fitness
        FitScaler       : float
        SpinWheel       : (Centroid*float)[]
        ParmDefs        : Parm[]
    }

let MAX_INDVS = 1000
let cfact xs k =  KMeansClustering.randomCentroids Probability.RNG.Value xs k |> List.map (fun (x:float[])->x,[])
let cdist (x,_) y = KMeansClustering.euclidean x y
let cavg (c,_) xs = (KMeansClustering.avgCentroid c xs),xs

let log cntrds = cntrds |> Seq.map (fun c -> c.Center) |> Seq.toList |> Metrics.MetricMsg.TopoState |> Metrics.postAll

let toCentroid state (c,members) =
    let lbest = members |> Seq.maxBy (fun ps -> (state.Fitness.Value ps) * state.FitScaler)
    {
        Center = c
        Count  = Seq.length members
        Best = lbest
        BestFit = state.Fitness.Value lbest
    }

let updateClusters state voters =
    let voters = voters |> Seq.map (fun indv -> toMarker indv)
    let vns = 
        Seq.append state.CIndvs voters 
        |> Seq.sortByDescending (fun i-> state.FitScaler * i.MFitness) 
        |> Seq.truncate MAX_INDVS 
        |> Seq.toArray
    let parmsArray = vns |> Array.map(fun i->i.MParms)
    // type CentroidsFactory<'a> = 'a seq -> int -> Centroid<'a> seq
    let k = match vns.Length with x when x < 10 -> 2 | x when x < 20 -> 4 | x when x < 100 -> 5 | x when x < 500 -> 7 | _ -> 10
    let kcntrods,_ = KMeansClustering.kmeans cdist cfact cavg  parmsArray k
    let cntrds = kcntrods |> Seq.filter (fun (_,ls)->List.isEmpty ls |> not) |> Seq.map (toCentroid state) |> Seq.toList
    let _,wheel = cntrds |> Seq.map (fun c->c,float c.Count) |> Seq.toArray |> Probability.createWheel

    #if _LOG_
    log cntrds
    #endif

    { state with Centroids = cntrds; SpinWheel=wheel}

let TOPOGRAPHICAL_RANGE_SCALER = 0.5

let initialState parmDefs isBetter fitness =
    {
        IsBetter    = isBetter
        Centroids   = []
        CIndvs      = [||]
        Fitness     = fitness
        FitScaler   = if isBetter 1. 0. then 1. else -1.
        SpinWheel   = [||]
        ParmDefs    = parmDefs
    }

let construct state fAccept fInfluence : KnowledgeSource<_> =
    {
        Type        = Topgraphical
        Accept      = fAccept fInfluence state
        Influence   = fInfluence state
    }

let rec defaultAcceptance fInfluence state envChanged  (voters:Individual<_> array) =

    let state =
        match envChanged with
        | Adjust -> initialState state.ParmDefs state.IsBetter state.Fitness
        | _      -> state

    let state = updateClusters state voters
    voters,construct state defaultAcceptance fInfluence

let defaultInfluence state _ s (indv:Individual<_>) =
    //mutation
    let cntrd = Probability.spinWheel state.SpinWheel 
    let p2 = cntrd.Best
    let updateParms = indv.Parms
    p2 |> Array.iteri (fun i p -> evolveP TOPOGRAPHICAL_RANGE_SCALER s eSigma updateParms i state.ParmDefs.[i] p)
    indv
    
let create parmDefs optKind (fitness:Fitness) =
    let state = initialState parmDefs (CAUtils.comparator optKind) fitness
    construct state defaultAcceptance defaultInfluence
