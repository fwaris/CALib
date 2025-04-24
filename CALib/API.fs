namespace CALib
open CA

module API =
    let inline makeCA fitness envChgSensitivity optKind pop bspace influence network =
        {
            Population           = pop
            Network              = network
            BeliefSpace          = bspace
            Acceptance           = CARunner.acceptance 0.25 (CAUtils.mult optKind)
            Influence            = influence
            Update               = CARunner.update
            Fitness              = fitness
            Optimization         = optKind
            EnvChngSensitivity   = envChgSensitivity
        }

/// Configuration of Cultural Algorithms BeliefSpace
type BeliefSpace = 
    ///Accurate but may be expensive to evalute for high dimensional spaces 
    | Default        
    ///A mix of default Domain KS (that calculates parameter slopes) and differential-evolution-based Domain KS
    | Hybrid 
    ///Only use differential-evolution Doman KS
    | DifferentialEvolution

type API() = 

    static member initCA(parms:Parm[], fitness:float[]->float, optKind:OptimizationKind, ?popSize, ?beliefSpace:BeliefSpace) =
        let popSize = defaultArg popSize 36
        let beliefSpace =  defaultArg beliefSpace Default
        let envChgSnstvty = Insensintive 
        let maybeInvalid = parms |> Array.tryFind(function F(v,l,h) -> not( h>=l && v >= l && v <= h) | I(v,l,h) -> not(h>=l && v >= l && v <= h))
        match maybeInvalid with Some p -> failwith $"Invalid parameter range or value {p}" | _ -> ()
        let fitness:Fitness = ref fitness
        let network = CAUtils.hexagonNetworkViz
        let createBsp = 
            match beliefSpace with
            | Default -> CARunner.defaultBeliefSpace
            | Hybrid -> CARunner.deHybridBeliefSpace
            | DifferentialEvolution -> CARunner.deBeliefSpace
        let bsp = createBsp parms optKind fitness
        let pop = CAUtils.createPop (CAUtils.baseKsInit bsp) parms popSize true
        let pop = pop |> KDStagHuntStatic.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
        let influence = KDStagHuntStatic.influence None 5  bsp pop
        let ca = API.makeCA fitness envChgSnstvty optKind pop bsp influence network
        let step =  {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0; IBest=ref None}
        step

    static member Step(step, ?maxParallelism:int) = CARunner.step maxParallelism false step  2