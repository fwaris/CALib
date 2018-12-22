module Runs.Environment
open Runs.Types
open CA
open DF1
open System.IO
open CAUtils

let parmDefs = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

let defaultComparator = CAUtils.Maximize
let defaultNetwork = CAUtils.hexagonNetworkViz
let defaultMaxBest = 2
let SEG_RAD = 2


let inline makeCA fitness comparator pop bspace kd influence network =
        {
            Population           = pop
            Network              = network
            KnowlegeDistribution = kd
            BeliefSpace          = bspace
            Acceptance           = CARunner.acceptance 0.25 comparator
            Influence            = influence
            Update               = CARunner.update
            Fitness              = fitness
            Comparator           = comparator
        }


////let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
//let createBeliefSpace fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
//let inline createPop size bsp parms init = CAUtils.createPop (init bsp) parms size true

let inline sqr x = x * x

let step envChanged st = CARunner.step envChanged st 2

let vmx = (0.2, 0.9)

let createEnv rsc a =
  let aH = if rsc.ChangeHeight then Some a else None
  let aR = if rsc.ChangeRadius then Some a else None
  let aC = if rsc.ChangeLoc then Some a else None
  let w = createWorld rsc.NumCones 2 (5.,15.) (20., 10.) aR aH aC 
  let (c,f) = landscape w
  {W=w; M=c; F=f; EnvChangeCount=0}

let changeEnv ws =
  let w = updateWorld ws.W
  let (c,f) = landscape w
  {W=w; M=c; F=f; EnvChangeCount = ws.EnvChangeCount + 1}
  
let saveEnv rsc id (ws:WorldState) =
  let fn = sprintf "Env_%s_%d.env" id (System.DateTime.Now.ToFileTime())
  let path = Path.Combine(rsc.SaveFolder,fn)
  DF1.saveEnv path  ws.W.Cones
    
let getNetwork id = 
  let networks = [Square,CAUtils.squareNetwork; Hexagon, CAUtils.hexagonNetworkViz; Octagon, CAUtils.octagonNetwork] |> Map.ofList
  networks.[id]



