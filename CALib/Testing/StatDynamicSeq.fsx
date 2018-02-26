#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load @"..\DF1.fs"
#load @"..\Utilities\TraceCharts.fs"
#load @"..\Utilities\VizUtils.fs"
#load "..\Utilities\Viz.fs"
#load @"..\Utilities\VizLandscape.fs"

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

//let FitnessHlder : Fitness = ref (fun xs -> 0.)

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
  let w = createWorld 144 2 (5.,15.) (20., 10.) None None (Some a) 
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
  let solFound = dist < 0.01
  solFound,dist,st

type Ret = {Run:int; I:string; R:int; A:float; C:int; Max:float; S:float; D:float}
let MAX_GEN = 5000
let saveFolder = @"D:\repodata\calib\dynstats"

let saveLandscape w r =
  let (c,f) = landscape w
  let bg = VizLandscape.gen (c,f)
  let fn = sprintf "%s-%d-%d-%f-%d-%f.png" r.I r.Run r.R r.A r.C r.Max
  let path = Path.Combine(saveFolder,fn)
  bg.Save path  


let rec findSol (st,ws) d envCh =
  let ws = 
    if envCh then 
      let ws = changeEnv ws 
      st.CA.Fitness := ws.F
      ws
    else 
      ws
  let solFound,dist,st = run (st,ws,envCh)
  if dist < d then
    printfn "dist: %f @ %d" dist st.Count
  if solFound then
    printfn "Sol found @%d for %s"  st.Count ws.Id
    st
  elif st.Count > MAX_GEN then
    printfn "MAX_GEN  for %s" ws.Id
    st
  else
    findSol (st,ws) dist false

let collecStats run i id segF ca =
  a_values 
  |> List.map (fun a ->
    let ws = createEnv id a
    ca.Fitness := ws.F
    let st = {CA=ca; Best=[]; Count=0; Progress=[]}
    let st = findSol (st,ws) 10.0 false
    let seg = 
      Social.segregation                                    //Schelling-like segregation measure
          2                                                 //radius of neighborhood
          (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
          Social.ksSegments                                 //list of segments
          st.CA                                             //current state of CA
          segF                                              //function to return segment for each individual
    let dffsn =  Social.diffusion st.CA
    let r = {Run=run; I=id; R= i; A=a; C=st.Count; S=seg; D=dffsn; Max=st.Best.[0].MFitness}
    if st.Count > MAX_GEN then
      saveLandscape ws.W r
    r)
    

let ipdStats(run) =
  let f : Fitness = ref (fun xs -> 0.)
  let startCA = kdIpdCA vmx f comparator parmDefs
  let id = "IPD"
  let ksf = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)
  [for i in 1 .. 50 -> collecStats run i id ksf startCA]

let wtdStats(run) =
  let f : Fitness = ref (fun xs -> 0.)
  let startCA = kdWeightedCA f comparator parmDefs
  let id = "WTD"
  [for i in 1 .. 50 -> collecStats run i id Social.baseSeg startCA]

let ipdS = [for run in 1..30 -> ipdStats(run)]
let wtdS = [for run in 1..30 -> wtdStats(run)]

let saveStates name stats =
  let stats = stats |> List.collect yourself |> List.collect yourself
  use fn = new StreamWriter(File.OpenWrite(Path.Combine(saveFolder,name)))
  fn.WriteLine("I\tR\tA\tC\tMax\tS\tD")
  stats |> List.iter(fun r->
    let line = sprintf "%s\t%d\t%f\t%d\t%f\t%f\t%f" r.I r.R r.A r.C r.Max r.S r.D
    fn.WriteLine(line)
    )
  fn.Close() |> ignore

saveStates (sprintf "IPD%d.txt" (System.DateTime.Now.ToFileTime()))  ipdS
saveStates (sprintf "WTD%d.txt" (System.DateTime.Now.ToFileTime()))  wtdS

