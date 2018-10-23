module Runs.StatsDynamic
open CAUtils
open TestEnv
open CA
open DF1
open System.IO
open System
open Metrics

type RunsConfig = 
  {
    SaveFolder    : string
    RunToMax      : bool
    CalcSocMetrics : bool
    MaxGen        : int
    NumLandscapes : int
    Samples       : int
    DistTh        : float
    AValues       : float list
    ChangeHeight  : bool
    ChangeRadius  : bool
    ChangeLoc     : bool
  }

//let SAVE_FOLDER =  @"D:\repodata\calib\dsst_stats"

//let RUN_TO_MAX = true
//let CALC_SOC_METRICS = true
//let MAX_GEN = 500
//let NUM_LANDSCAPES = 50
//let SAMPLES = 30
//let DIST_TH = 0.001
////let A_VALUES = [1.01; 2.8; 3.35; 3.5; 3.99] // [1.0; 3.5; 3.99]
//let A_VALUES = [1.0; 1.4; 1.8; 2.2; 2.6; 3.1; 3.2;3.3;3.4;3.5;3.6;3.7;3.8;3.9]
  
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

type WorldState = {Id:string; W:World; M:Cone; F:float[]->float; EnvChangeCount:int}
type NetId = Square | Hexagon | Octagon
type Config = {Id:string; Run:int; Net:NetId; A:float}
type Ret = {ConfigRun:int; Id:string; LandscapeNum:int; A:float; GenCount:int; Max:float; Seg:float; Dffsn:float; Net:string}

let createEnv rsc id a =
  let aH = if rsc.ChangeHeight then Some a else None
  let aR = if rsc.ChangeRadius then Some a else None
  let aC = if rsc.ChangeLoc then Some a else None
  let w = createWorld 500 2 (5.,15.) (20., 10.) aR aH aC 
  let (c,f) = landscape w
  {Id=id; W=w; M=c; F=f; EnvChangeCount=0}

let changeEnv ws =
  let w = updateWorld ws.W
  let (c,f) = landscape w
  {Id=ws.Id; W=w; M=c; F=f; EnvChangeCount = ws.EnvChangeCount + 1}
  

let saveEnv rsc id (ws:WorldState) =
  let fn = sprintf "Env_%s_%d.env" id (System.DateTime.Now.ToFileTime())
  let path = Path.Combine(rsc.SaveFolder,fn)
  DF1.saveEnv path  ws.W.Cones
    
let getNetwork id = 
  let networks = [Square,CAUtils.squareNetwork; Hexagon, CAUtils.hexagonNetworkViz; Octagon, CAUtils.octagonNetwork] |> Map.ofList
  networks.[id]

type RunState<'k> =
  {
    Id        : string
    A         : float
    PrimKS    : 'k->Knowledge
    Ws        : WorldState
    EnvCh     : bool
    Step      : TimeStep<'k>
    Landscape : int
    SampleNum : int
    StrWComm  : StreamWriter
    StrWRun   : StreamWriter
  }

let writeRun rst =
  let ft = match rst.Step.Best with [] -> 0.0 | x::_ -> x.MFitness
  let line = String.Join("\t",rst.Id,rst.A,rst.SampleNum,rst.Landscape,rst.Step.Count,ft,rst.Ws.EnvChangeCount,rst.Ws.M.H)
  rst.StrWRun.WriteLine line

let rec runToSol rsc rst = //id a primKs ws envCh st gens =
  async {
  if rst.Step.Count > rsc.MaxGen then 
    printfn "MAx_GEN Id=%s A=%f Lndscp=%d Sample=%d" rst.Id rst.A rst.Landscape rst.SampleNum
    if rsc.RunToMax then () else saveEnv rsc rst.Id rst.Ws //save environment to analyze later
    return false,rst.Step
  else
    let st = step rst.EnvCh rst.Step
    let rst = {rst with Step=st; EnvCh=false}
    //do! Async.Sleep 100
    writeRun rst
    //Community.logCluster rst.StrWComm rst.Id rst.A rst.Step.Count rst.Landscape rst.PrimKS rst.Step.CA.Network rst.Step.CA.Population
    let (bfit,gb) = best st
    let dist = Array.zip gb rst.Ws.M.L |> Seq.sumBy (fun (a,b) -> sqr (a - b)) |> sqrt 
    let solFound = dist < rsc.DistTh
    if solFound && not rsc.RunToMax then 
      printfn "sol @ %d %s" st.Count rst.Id
      return true,st
    else
      return! runToSol rsc rst
  }

let statRec rsc i ipdSol st  (config:Config) segF =
  let seg,dffsn =
    if rsc.CalcSocMetrics then
      let seg = 
        Social.segregation                                    //Schelling-like segregation measure
            2                                                 //radius of neighborhood
            (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
            Social.ksSegments                                 //list of segments
            st.CA                                             //current state of CA
            segF                                              //function to return segment for each individual
      let dffsn =  Social.diffusion st.CA
      seg,dffsn
    else
      -1.0,-1.0
  {
    ConfigRun=config.Run
    Id=config.Id; LandscapeNum=i
    A=config.A; GenCount=st.Count
    Seg=seg
    Dffsn=dffsn
    Max=st.Best.[0].MFitness
    Net=sprintf "%A" config.Net
  }

let runConfig rsc fstrCommunity fstrRun  numLandscapes (config:Config) =

  //printfn "config %A %A %A" config.A config.Id config.Run
  let ws = createEnv rsc config.Id config.A
  let f : Fitness = ref ws.F
  //init pop and belief space
  let wtdBsp = bsp f parmDefs comparator
  let wtdPop = createPop wtdBsp parmDefs CAUtils.baseKsInit
  //
  let ipdPop = wtdPop |> KDIPDGame.initKS |> Array.map (fun x-> {x with Parms=Array.copy x.Parms})
  let ipdBsp = bsp f parmDefs comparator
  //
  let shPop = wtdPop |> KDStagHunt.initKS |> Array.map (fun x-> {x with Parms=Array.copy x.Parms})
  let shBsp = bsp f parmDefs comparator
  //
  let stkPop = wtdPop |> KDStackelberg.initKS |> Array.map (fun x-> {x with Parms=Array.copy x.Parms})
  let stkBsp = bsp f parmDefs comparator
//ipd kd
  let ada = KDIPDGame.Geometric(0.9,0.01)
  let ipdKd,ipdInf = ipdKdist Domain ada vmx comparator ipdPop 
  //wtd kd 
  let ksSet = CAUtils.flatten wtdBsp |> List.map (fun ks->ks.Type) |> set
  let wtdKd = wtdMajorityKdist comparator ksSet
  //sh kd
  let shKd = KDStagHunt.knowledgeDist None 5 comparator shBsp shPop
  //stk dk
  let stkKd = KDStackelberg.knowledgeDist comparator 
 //CA
  let ipdCA = makeCA f comparator ipdPop ipdBsp ipdKd ipdInf (getNetwork config.Net)
  let wtdCA = makeCA f comparator wtdPop wtdBsp wtdKd KDWeightedMajority.wtdMajorityInfluence (getNetwork config.Net)
  let shCA = makeCA f comparator shPop shBsp shKd KDStagHunt.shInfluence (getNetwork config.Net)
  let stkCA = makeCA f comparator stkPop stkBsp stkKd KDStackelberg.stkInfluence (getNetwork config.Net)

  let ipdSt =  {CA=ipdCA; Best=[]; Count=0; Progress=[]}
  let ksf = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)

  let wtdSt =  {CA=wtdCA; Best=[]; Count=0; Progress=[]}

  let shSt = {CA=shCA; Best=[]; Count=0; Progress=[]}
  let shKs = (fun (x:Individual<KDStagHunt.ShKnowledge>) -> Social.ksNum (fst x.KS))

  let stkSt = {CA=stkCA; Best=[]; Count=0; Progress=[]}
  let stkKs = (fun (x:Individual<KDStackelberg.StkKnowledge>) -> Social.ksNum (fst x.KS))


  [for i in 1..numLandscapes do
    let ws = changeEnv ws  //change landscape using assigned A-value
    ipdCA.Fitness := ws.F  //update fitness functions
    wtdCA.Fitness := ws.F
    stkCA.Fitness := ws.F
    shCA.Fitness := ws.F

    //Note: population is not re-initialized therefore individuals retain locations from prior run
    let ipdRst = {Id= "IPD"; Landscape=i; A=config.A; PrimKS=Community.gamePrimKs; Ws=ws;
                  EnvCh=true; Step=ipdSt; StrWComm=fstrCommunity; StrWRun=fstrRun; SampleNum=config.Run}
    let wtdRst = {Id= "WTD"; Landscape=i; A=config.A; PrimKS=Community.basePrimKs; Ws=ws;
                  EnvCh=true; Step=wtdSt; StrWComm=fstrCommunity; StrWRun=fstrRun; SampleNum=config.Run}
    let shRst = {Id= "SH"; Landscape=i; A=config.A; PrimKS=Community.fstPrimKs; Ws=ws;
                  EnvCh=true; Step=shSt; StrWComm=fstrCommunity; StrWRun=fstrRun; SampleNum=config.Run}
    let stkRst = {Id= "STK"; Landscape=i; A=config.A; PrimKS=Community.fstPrimKs; Ws=ws;
                  EnvCh=true; Step=stkSt; StrWComm=fstrCommunity; StrWRun=fstrRun; SampleNum=config.Run}

    let ipdSol,ipdSt = runToSol rsc ipdRst |> Async.RunSynchronously
    let wtdSol,wtdSt = runToSol rsc wtdRst |> Async.RunSynchronously 
    let shSol,shSt = runToSol rsc shRst    |> Async.RunSynchronously 
    let stkSol,stkSt = runToSol rsc stkRst |> Async.RunSynchronously 

    yield statRec rsc i ipdSol ipdSt {config with Id="IPD"} ksf
    yield statRec rsc i wtdSol wtdSt {config with Id="WTD"} Social.baseSeg
    yield statRec rsc i shSol shSt {config with Id="SH"} shKs
    yield statRec rsc i stkSol stkSt {config with Id="STK"} stkKs
  ]

let initLogging rsc =
  if not <| Directory.Exists rsc.SaveFolder then Directory.CreateDirectory rsc.SaveFolder |> ignore

let run rsc =
  seq{
    use fstrCommunity = Path.Combine(rsc.SaveFolder,"comm_base_runs.txt") |> Community.initLog
    use fstrRun = Path.Combine(rsc.SaveFolder,"res_run.txt") |> File.CreateText
    for a in rsc.AValues do
      //for n in [Square; Hexagon; Octagon] do
      for n in [Hexagon] do
        for i in 1..rsc.Samples do
          let config = {Config.Run=i; Config.A=a; Config.Net=n; Config.Id=""}
          yield! runConfig rsc fstrCommunity fstrRun rsc.NumLandscapes config}

let saveStates rsc name stats =
  use fn = new StreamWriter(File.OpenWrite(Path.Combine(rsc.SaveFolder,name)))
  fn.WriteLine("ConfigRun\tId\tLandscapeNum\tA\tGenCount\tMax\tSeg\tDffsn\tNet")
  stats |> List.iter(fun r->
    let line = 
      sprintf "%d\t%s\t%d\t%f\t%d\t%f\t%f\t%f\t%s" 
        r.ConfigRun r.Id r.LandscapeNum r.A r.GenCount r.Max r.Seg r.Dffsn r.Net
    fn.WriteLine(line)
    )
  fn.Close() |> ignore

//uncomment to record video showing community detection in social network
//let obsDisp,enc = Community.visCommunity @"D:\repodata\calib\comm\test1.mp4" KDIPDGame.obsNetW

//let stats = run() |> Seq.take 1 |> Seq.toList
//saveStates  (sprintf "Stats%d.txt" (System.DateTime.Now.ToFileTime())) stats

let start rsc =
  initLogging rsc
  async {
    let stats = run rsc |> Seq.toList
    saveStates rsc  (sprintf "Stats%d.txt" (System.DateTime.Now.ToFileTime())) stats
    printfn "Done stats"
  } |> Async.Start

(* stop video recording (does not stop CA run) killing fsi causes video corruption

obsDisp.Dispose()
enc.Release()

*)