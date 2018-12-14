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
open System
open Metrics
open OpenCvSharp
open Tracing

let RUN_TO_MAX = true
let CALC_SOC_METRICS = true
let MAX_GEN = 250
let NUM_LANDSCAPES = 50
let SAMPLES = 30
let DIST_TH = 0.001
//let A_VALUES = [1.01; 2.8; 3.35; 3.5; 3.99] // [1.0; 3.5; 3.99]
//let A_VALUES = [1.0; 1.4; 1.8; 2.2; 2.6; 3.1; 3.2;3.3;3.4;3.5;3.6;3.7;3.8;3.9]
let A_VALUES = [3.5;5.7]

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

type Ret = {ConfigRun:int; Id:string; LandscapeNum:int; A:float; 
            GenCount:int; Max:float; Seg:float; Dffsn:float; Net:string
            IndvSeg   : float[]
            IndvDfsn  : float[]
            }

let createEnv id a =
  let w = createWorld 500 2 (5.,15.) (20., 10.) None None (Some a) //loc
  //let w = createWorld 500 2 (5.,15.) (20., 10.) None (Some a) None //height
  //let w = createWorld 500 2 (5.,15.) (20., 10.) (Some a) None None //radius
  let (c,f) = landscape w
  {Id=id; W=w; M=c; F=f; EnvChangeCount=0}

let changeEnv ws =
  let w = updateWorld ws.W
  let (c,f) = landscape w
  {Id=ws.Id; W=w; M=c; F=f; EnvChangeCount = ws.EnvChangeCount + 1}
  
let saveFolder =  @"D:\repodata\calib\dsst_stats"
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

let segAt ca fSeg indv = Social.segregationAt                        //Schelling-like segregation measure
                            2                                       //radius of neighborhood
                            (1.0 / float Social.ksSegments.Length)  //proportion of each segment or group at start
                            Social.ksSegments                       //list of segments
                            ca                                      //current state of CA
                            fSeg
                            indv

let dfsnAt ca p = Social.diffusionAt ca p


let rec runToSol rst = //id a primKs ws envCh st gens =
  async {
  if rst.Step.Count > MAX_GEN then 
    printfn "MAx_GEN Id=%s A=%f Lndscp=%d Sample=%d" rst.Id rst.A rst.Landscape rst.SampleNum
    if RUN_TO_MAX then () else saveEnv rst.Id rst.Ws //save environment to analyze later
    return false,rst.Step
  else
    let st = step rst.EnvCh rst.Step
    let rst = {rst with Step=st; EnvCh=false}
    //do! Async.Sleep 100
    writeRun rst
    //Community.logCluster rst.StrWComm rst.Id rst.A rst.Step.Count rst.Landscape rst.PrimKS rst.Step.CA.Network rst.Step.CA.Population
    let (bfit,gb) = best st
    let dist = Array.zip gb rst.Ws.M.L |> Seq.sumBy (fun (a,b) -> sqr (a - b)) |> sqrt 
    let solFound = dist < DIST_TH
    if solFound && not RUN_TO_MAX then 
      printfn "sol @ %d %s" st.Count rst.Id
      return true,st
    else
      return! runToSol rst
  }

let statRec i ipdSol st  (config:Config) segF =
  let seg,dffsn =
    if CALC_SOC_METRICS then
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
    IndvSeg = st.CA.Population |> Array.map (segAt st.CA segF)
    IndvDfsn = st.CA.Population |> Array.map (dfsnAt st.CA)
  }

let runConfig fstrCommunity fstrRun  numLandscapes (config:Config) =

  //printfn "config %A %A %A" config.A config.Id config.Run
  let ws = createEnv config.Id config.A
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

    let ipdSol,ipdSt = runToSol ipdRst |> Async.RunSynchronously
    let wtdSol,wtdSt = runToSol wtdRst |> Async.RunSynchronously 
    let shSol,shSt = runToSol shRst    |> Async.RunSynchronously 
    let stkSol,stkSt = runToSol stkRst |> Async.RunSynchronously 

    yield statRec i ipdSol ipdSt {config with Id="IPD"} ksf
    yield statRec i wtdSol wtdSt {config with Id="WTD"} Social.baseSeg
    yield statRec i shSol shSt {config with Id="SH"} shKs
    yield statRec i stkSol stkSt {config with Id="STK"} stkKs
  ]

let run() =
  seq{
    use fstrCommunity = Path.Combine(saveFolder,"comm_base_runs.txt") |> Community.initLog
    use fstrRun = Path.Combine(saveFolder,"res_run.txt") |> File.CreateText
    for a in A_VALUES do
      //for n in [Square; Hexagon; Octagon] do
      for n in [Hexagon] do
        for i in 1..SAMPLES do
          let config = {Config.Run=i; Config.A=a; Config.Net=n; Config.Id=""}
          yield! runConfig fstrCommunity fstrRun NUM_LANDSCAPES config}

let saveStates name stats =

    let writeStats() =
      use fn = new StreamWriter(File.OpenWrite(Path.Combine(saveFolder,name)))
      fn.WriteLine("ConfigRun\tId\tLandscapeNum\tA\tGenCount\tMax\tSeg\tDffsn\tNet")
      stats |> List.iter(fun r->
        let line = 
          sprintf "%d\t%s\t%d\t%f\t%d\t%f\t%f\t%f\t%s" 
            r.ConfigRun r.Id r.LandscapeNum r.A r.GenCount r.Max r.Seg r.Dffsn r.Net
        fn.WriteLine(line)
        )
      fn.Close()
    
    let writeSoc() =
        use fn = new StreamWriter(File.OpenWrite(Path.Combine(saveFolder,"soc_",name)))
        fn.WriteLine("ConfigRun\tId\tLandscapeNum\tA\tGenCount\tMax\tSeg\tDffsn\tNet\tIndvSeg\tIndvDffsn")
        stats |> List.iter(fun r->
        let line = 
            sprintf "%d\t%s\t%d\t%f\t%d\t%f\t%f\t%f\t%s" 
                r.ConfigRun r.Id r.LandscapeNum r.A r.GenCount r.Max r.Seg r.Dffsn r.Net
        fn.Write(line)
        fn.Write("\t")
        r.IndvSeg |> Array.iter (fun x -> fn.Write(x); fn.Write("|"))
        fn.Write("\t")
        r.IndvDfsn |> Array.iter (fun x -> fn.Write(x); fn.Write("|"))
        fn.WriteLine()
        )
        fn.Close()
   
    writeStats()
    writeSoc()

//uncomment to record video showing community detection in social network
//let obsDisp,enc = Community.visCommunity @"D:\repodata\calib\comm\test1.mp4" KDIPDGame.obsNetW

//let stats = run() |> Seq.take 1 |> Seq.toList
//saveStates  (sprintf "Stats%d.txt" (System.DateTime.Now.ToFileTime())) stats

async {
  let stats = run() |> Seq.toList
  saveStates  (sprintf "Stats%d.txt" (System.DateTime.Now.ToFileTime())) stats
} |> Async.Start

(* stop video recording (does not stop CA run) killing fsi causes video corruption

obsDisp.Dispose()
enc.Release()

*)