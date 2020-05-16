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

type API() = 

    static member initCA(parms:Parm[], fitness:float[]->float, optKind:OptimizationKind, ?popSize, ?useDE) =
        let popSize = defaultArg popSize 36
        let useDE =  defaultArg useDE false
        let envChgSnstvty = Insensintive 
        let fitness:Fitness = ref fitness
        let network = CAUtils.hexagonNetworkViz
        let createBsp = if useDE then CARunner.deHybridBeliefSpace else CARunner.defaultBeliefSpace
        let bsp = createBsp parms optKind fitness
        let pop = CAUtils.createPop (CAUtils.baseKsInit bsp) parms popSize true
        let pop = pop |> KDStagHuntStatic.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
        let influence = KDStagHuntStatic.influence None 5  bsp pop
        let ca = API.makeCA fitness envChgSnstvty optKind pop bsp influence network
        let step =  {CA=ca; Best=[]; Count=0; Progress=[]; EnvChngCount=0; IBest=ref None}
        step

    static member Step step = CARunner.step false step  2