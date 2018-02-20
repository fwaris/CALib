#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load @"..\DF1.fs"

open CAUtils
open TestEnv
open CA
open DF1
open System.IO
open Metrics
open OpenCvSharp

let parmDefs = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

let FitnessHlder : Fitness = ref (fun xs -> 0.)

let comparator  = CAUtils.Maximize

//let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
let bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
let inline createPop bsp parms init = CAUtils.createPop (init bsp) parms 72 true

let kdIpdCA vmx ftnss cmprtr parmDefs  = 
    let b = bsp ftnss parmDefs cmprtr
    let pop = createPop b parmDefs CAUtils.baseKsInit |> KDIPDGame.initKS
    let ada = KDIPDGame.Geometric(0.9,0.01)
    let kd = ipdKdist ada vmx cmprtr pop 
    makeCA ftnss cmprtr pop b kd KDIPDGame.ipdInfluence

let kdWeightedCA f c p  = 
    let bsp = bsp f p c
    let ksSet = CAUtils.flatten bsp |> List.map (fun ks->ks.Type) |> set
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (wtdMajorityKdist c ksSet) KDWeightedMajority.wtdMajorityInfluence

let inline sqr x = x * x

let step envChanged st = CARunner.step envChanged st 2

let vmx = (0.2, 0.9)

//let startCA = kdIpdCA vmx FitnessHlder comparator parmDefs
//let startCA = kdWeightedCA fitness comparator parmDefs
//let startStep = {CA=startCA; Best=[]; Count=0; Progress=[]}

let primarkyKS (x:obj) =
    match x with 
    | :? (KDIPDGame.PrimaryKS * Map<Knowledge,float>) as ks -> (fst ks).KS
    | :? Knowledge as k -> k
    | _-> failwithf "not handled"

let secondaryKS (x:obj) =
    match x with 
    | :? (KDIPDGame.PrimaryKS * Map<Knowledge,float>) as ks -> snd ks |> Some
    | :? Knowledge as k -> None
    | _-> failwithf "not handled"

;;
let a_values = [1.0; 3.1; 3.5; 3.9]

type WorldState = {Id:string; W:World; M:Cone; F:float[]->float; EnvChangeCount:int}

let createEnv id a =
  let w = createWorld 100 2 (5.,15.) (20., 10.) None None (Some a) 
  let (c,f) = landscape w
  {Id=id; W=w; M=c; F=f; EnvChangeCount=0}

let changeEnv ws =
  let w = updateWorld ws.W
  let (c,f) = landscape w
  {Id=ws.Id; W=w; M=c; F=f; EnvChangeCount = ws.EnvChangeCount + 1}
  
let inline run<'a> (st:TimeStep<'a>,ws,envCh) =
  let st = step envCh st
  let (bfit,gb) = best st
  let dist = Array.zip gb ws.M.L |> Seq.sumBy (fun (a,b) -> sqr (a - b)) |> sqrt 
  printfn "%A" dist
  let solFound = dist < 0.001
  solFound,st

let rec findSol (st,ws) envCh =
  let ws = 
    if envCh then 
      let ws = changeEnv ws 
      st.CA.Fitness := ws.F
      ws
    else 
      ws
  let solFound,st = run (st,ws,envCh)
  if solFound then
    printfn "Sol found @%d for %s"  st.Count ws.Id
    st
  else
    findSol (st,ws) false

type Ret = {I:string; R:int; A:float; C:int; S:float; D:float}

let collecStats i id segF ca =
  a_values 
  |> List.map (fun a ->
    let ws = createEnv id a
    let f = ref ws.F
    let st = {CA={ca with Fitness=f}; Best=[]; Count=0; Progress=[]}
    let st = findSol (st,ws) false
    let seg = 
      Social.segregation                                    //Schelling-like segregation measure
          2                                                 //radius of neighborhood
          (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
          Social.ksSegments                                 //list of segments
          st.CA                                             //current state of CA
          segF                                              //function to return segment for each individual
    let dffsn =  Social.diffusion st.CA
    {I=id; R= i; A=a; C=st.Count; S=seg; D=dffsn})

let ipdStats() =
  let startCA = kdIpdCA vmx FitnessHlder comparator parmDefs
  let id = "IPD"
  let ksf = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)
  [for i in 1 .. 50 -> collecStats i id ksf startCA]

let wtdStats() =
  let startCA = kdWeightedCA FitnessHlder comparator parmDefs
  let id = "IPD"
  [for i in 1 .. 50 -> collecStats i id Social.baseSeg startCA]

let ipdS = ipdStats()
let wtdS = wtdStats()