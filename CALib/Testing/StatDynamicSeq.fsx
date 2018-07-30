#load "TestEnv.fsx"
#load "SetupVideo.fsx"
#load @"..\DF1.fs"
#load @"..\Utilities\TraceCharts.fs"
#load @"..\Utilities\VizUtils.fs"
#load "..\Utilities\VizNetwork.fs"
#load @"..\Utilities\VizLandscape.fs"
#load @"..\Utilities\FPGrowth.fs"
#load @"..\Utilities\Community.fs"

open CAUtils
open TestEnv
open CA
open DF1
open System.IO
open Metrics
open OpenCvSharp

let RUN_TO_MAX = true
let MAX_GEN = 500
let NUM_LANDSCAPES = 50
let SAMPLES = 30
let DIST_TH = 0.001

let parmDefs = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

let comparator  = CAUtils.Maximize

//let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
let bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
let inline createPop bsp parms init = CAUtils.createPop (init bsp) parms 72 true

let inline sqr x = x * x

let step envChanged st = CARunner.step envChanged st 2

let vmx = (0.2, 0.9)

let a_values = [1.0; 3.1; 3.5; 3.99]

type WorldState = {Id:string; W:World; M:Cone; F:float[]->float; EnvChangeCount:int}
type NetId = Square | Hexagon | Octagon
type Config = {Id:string; Run:int; Net:NetId; A:float}
type Ret = {ConfigRun:int; Id:string; LandscapeNum:int; A:float; GenCount:int; Max:float; Seg:float; Dffsn:float; Net:string}

let createEnv id a =
  let w = createWorld 144 2 (5.,15.) (20., 10.) None None (Some a) 
  let (c,f) = landscape w
  {Id=id; W=w; M=c; F=f; EnvChangeCount=0}

let changeEnv ws =
  let w = updateWorld ws.W
  let (c,f) = landscape w
  {Id=ws.Id; W=w; M=c; F=f; EnvChangeCount = ws.EnvChangeCount + 1}
  
let saveFolder = @"d:\repodata\calib\dynstatsNetMax"
if not <| Directory.Exists saveFolder then Directory.CreateDirectory saveFolder |> ignore

let saveLandscape w r =
  let (c,f) = landscape w
  let bg = VizLandscape.gen (c,f)
  let fn = sprintf "%s-%d-%d-%f-%d-%f.png" r.Id r.ConfigRun r.LandscapeNum r.A r.GenCount r.Max
  let path = Path.Combine(saveFolder,fn)
  bg.Save path  

let saveEnv id (ws:WorldState) =
  let fn = sprintf "Env_%s_%d.env" id (System.DateTime.Now.ToFileTime())
  let path = Path.Combine(saveFolder,fn)
  DF1.saveEnv path  ws.W.Cones
    
let getNetwork id = 
  let networks = [Square,CAUtils.squareNetwork; Hexagon, CAUtils.hexagonNetworkViz; Octagon, CAUtils.octagonNetwork] |> Map.ofList
  networks.[id]

let str = Community.initLog @"D:\repodata\calib\comm\comm1.txt"

let rec runToSol id primKs ws envCh st gens =
  async {
  if gens > MAX_GEN then 
    printfn "MAx_GEN %s" id
    if RUN_TO_MAX then () else saveEnv id ws
    return false,st
  else
    let st = step envCh st
    //do! Async.Sleep 100
    Community.logCluster str id primKs st.CA.Network st.CA.Population
    let (bfit,gb) = best st
    let dist = Array.zip gb ws.M.L |> Seq.sumBy (fun (a,b) -> sqr (a - b)) |> sqrt 
    let solFound = dist < DIST_TH
    if solFound && not RUN_TO_MAX then 
      printfn "sol @ %d %s" st.Count id
      return true,st
    else
      return! runToSol id primKs ws false st (gens+1) 
  }

let statRec i ipdSol st  (config:Config) segF =
  let seg = 
    Social.segregation                                    //Schelling-like segregation measure
        2                                                 //radius of neighborhood
        (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
        Social.ksSegments                                 //list of segments
        st.CA                                             //current state of CA
        segF                                              //function to return segment for each individual
  let dffsn =  Social.diffusion st.CA
  {
    ConfigRun=config.Run
    Id=config.Id; LandscapeNum=i
    A=config.A; GenCount=st.Count
    Seg=seg
    Dffsn=dffsn
    Max=st.Best.[0].MFitness
    Net=sprintf "%A" config.Net
  }

let runConfig numLandscapes (config:Config) =
  let ws = createEnv config.Id config.A
  let f : Fitness = ref ws.F
  //init pop and belief space
  let wtdBsp = bsp f parmDefs comparator
  let wtdPop = createPop wtdBsp parmDefs CAUtils.baseKsInit
  let ipdPop = wtdPop |> KDIPDGame.initKS |> Array.map (fun x-> {x with Parms=Array.copy x.Parms})
  let ipdBsp = bsp f parmDefs comparator
  //ipd kd
  let ada = KDIPDGame.Geometric(0.9,0.01)
  let ipdKd = ipdKdist ada vmx comparator ipdPop 
  //wtd kd 
  let ksSet = CAUtils.flatten wtdBsp |> List.map (fun ks->ks.Type) |> set
  let wtdKd = wtdMajorityKdist comparator ksSet
  //CA
  let ipdCA = makeCA f comparator ipdPop ipdBsp ipdKd KDIPDGame.ipdInfluence (getNetwork config.Net)
  let wtdCA = makeCA f comparator wtdPop wtdBsp wtdKd KDWeightedMajority.wtdMajorityInfluence (getNetwork config.Net)
  
  let ipdSt =  {CA=ipdCA; Best=[]; Count=0; Progress=[]}
  let ksf = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)

  let wtdSt =  {CA=wtdCA; Best=[]; Count=0; Progress=[]}
  
  [for i in 1..numLandscapes do
    let ws = changeEnv ws
    ipdCA.Fitness := ws.F
    wtdCA.Fitness := ws.F
    let ipdSol,ipdSt = runToSol "IPD" Community.gamePrimKs ws true ipdSt 0 |> Async.RunSynchronously
    let wtdSol,wtdSt = runToSol "WTD" Community.basePrimKs ws true wtdSt 0 |> Async.RunSynchronously 
    yield statRec i ipdSol ipdSt {config with Id="IPD"} ksf
    yield statRec i wtdSol wtdSt {config with Id="WTD"} Social.baseSeg]

let run() =
  seq{
    for a in a_values do
      //for n in [Square; Hexagon; Octagon] do
      for n in [Hexagon] do
        for i in 1..SAMPLES do
          let config = {Config.Run=i; Config.A=a; Config.Net=n; Config.Id=""}
          yield! runConfig NUM_LANDSCAPES config}

let saveStates name stats =
  use fn = new StreamWriter(File.OpenWrite(Path.Combine(saveFolder,name)))
  fn.WriteLine("ConfigRun\tId\tLandscapeNum\tA\tGenCount\tMax\tSeg\tDffsn\tNet")
  stats |> List.iter(fun r->
    let line = 
      sprintf "%d\t%s\t%d\t%f\t%d\t%f\t%f\t%f\t%s" 
        r.ConfigRun r.Id r.LandscapeNum r.A r.GenCount r.Max r.Seg r.Dffsn r.Net
    fn.WriteLine(line)
    )
  fn.Close() |> ignore

//let obsDisp,enc = Community.visCommunity @"D:\repodata\calib\comm\test1.mp4" KDIPDGame.obsNetW

//let stats = run() |> Seq.take 1 |> Seq.toList
//saveStates  (sprintf "Stats%d.txt" (System.DateTime.Now.ToFileTime())) stats

async {
  let stats = run() |> Seq.take 1 |> Seq.toList
  str.Close()
  saveStates  (sprintf "Stats%d.txt" (System.DateTime.Now.ToFileTime())) stats
} |> Async.Start

(* stop video recording (does not stop CA run) killing fsi causes video corruption

obsDisp.Dispose()
enc.Release()



*)