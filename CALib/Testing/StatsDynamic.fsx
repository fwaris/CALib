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

let w = createWorld 500 2 (5.,15.) (20., 10.) None None (Some 3.1) |> ref
let m,f = DF1.landscape !w
let fitness = ref f
let maxCone = ref m
let envChangedCount = ref 0

let comparator  = CAUtils.Maximize

//let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
let bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
let inline createPop bsp parms init = CAUtils.createPop (init bsp) parms 360 true

let kdIpdCA vmx ftnss cmprtr parmDefs  = 
    let b = bsp ftnss parmDefs cmprtr
    let pop = createPop b parmDefs CAUtils.baseKsInit |> KDIPDGame.initKS
    let ada = KDIPDGame.Geometric(0.9,0.1)
    let kd = ipdKdist ada vmx cmprtr pop 
    makeCA ftnss cmprtr pop b kd KDIPDGame.ipdInfluence

//let kdlWeightedCA f c p = 
//    let bsp = bsp f p c
//    let pop = createPop bsp p CAUtils.baseKsInit
//    makeCA f c pop bsp (lWtdMajorityKdist c) CARunner.baseInfluence

let kdWeightedCA f c p  = 
    let bsp = bsp f p c
    let ksSet = CAUtils.flatten bsp |> List.map (fun ks->ks.Type) |> set
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (wtdMajorityKdist c ksSet) KDWeightedMajority.wtdMajorityInfluence

let inline sqr x = x * x

let step envChanged st = CARunner.step envChanged st 2

let vmx = (0.2, 0.9)

let startCA = kdIpdCA vmx fitness comparator parmDefs
//let startCA = kdWeightedCA fitness comparator parmDefs
let startStep = {CA=startCA; Best=[]; Count=0; Progress=[]}

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
let changeEnvironment() =
    async {
        w := updateWorld !w
        let (c,f) = landscape !w
        fitness := f
        maxCone := c
        envChangedCount := !envChangedCount + 1
    }

let go = ref true

let inline run<'a> (st:TimeStep<'a>) f =
    let envCh =
      if st.Count > 0 && st.Count % 100 = 0 && !envChangedCount < 4 then
        changeEnvironment() |> Async.RunSynchronously
        printf "env changed"
        true
      else 
        false
    let st = step envCh st
    let (bfit,gb) = best st
    let solFound = Array.zip gb maxCone.Value.L |> Seq.forall (fun (a,b) -> abs (a - b) < 0.01)
    if solFound then 
        if !envChangedCount >= 4 then
          go := false
        printfn "sol @ %d - B=%A - C=%A" st.Count (bfit,gb) m
    let maxHop = st.CA.Population.Length / 36 / 2
    let segs = [1..maxHop] |> List.map (fun i -> 
      Social.segregation                                    //Schelling-like segregation measure
          i                                                 //radius of neighborhood
          (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
          Social.ksSegments                                 //list of segments
          st.CA                                             //current state of CA
          f                                                 //funtion to return segment for each individual
      )
    let dffns = Social.diffusion st.CA
    printfn "gen %d" st.Count
    ((segs,dffns),st)
  
let runStats st ksf =
  let Avals = [3.1; 3.99]
  Avals |> List.map (fun a ->
    w := createWorld 500 2 (5.,15.) (20., 10.) None None (Some a) 
    let m,f = DF1.landscape !w
    fitness := f
    maxCone := m
    envChangedCount := 0
    //let st = {CA=startCA; Best=[]; Count=0; Progress=[]}
    let stats = st |> List.unfold (fun st -> let (stats,st) = run st ksf in if !go = false || st.Count > 2500 then None else Some (stats,st))
    a,stats)

let runIPD() =
  [0..30] |> List.map (fun i -> 
    let startCA = kdIpdCA vmx fitness comparator parmDefs
    let ksf = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)
    let stats = runStats {CA=startCA; Best=[]; Count=0; Progress=[]} ksf
    (i,stats))

let runWtd() =
  [0..30] |> List.map (fun i -> 
    let startCA = kdWeightedCA fitness comparator parmDefs
    let stats = runStats {CA=startCA; Best=[]; Count=0; Progress=[]} Social.baseSeg
    (i,stats))


let rIpd = runIPD()
let rWtd = runWtd()

  

