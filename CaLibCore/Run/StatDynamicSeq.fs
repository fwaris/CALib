module Runs.StatsDynamic
open CAUtils
open TestEnv
open CA
open DF1
open System.IO
open System
open Metrics
open FSharp.Collections.ParallelSeq
open Runs.Types

  
let parmDefs = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

let comparator  = CAUtils.Maximize

//let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
let bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
let inline createPop rsc bsp parms init = CAUtils.createPop (init bsp) parms rsc.PopulationSize true

let inline sqr x = x * x

let step envChanged st = CARunner.step envChanged st 2

let vmx = (0.2, 0.9)

let createEnv rsc id a =
  let aH = if rsc.ChangeHeight then Some a else None
  let aR = if rsc.ChangeRadius then Some a else None
  let aC = if rsc.ChangeLoc then Some a else None
  let w = createWorld rsc.NumCones 2 (5.,15.) (20., 10.) aR aH aC 
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

let socialMetrics st primKS =
    let segF i =  primKS i.KS |> Social.ksNum
    let seg = 
        Social.segregation                                    //Schelling-like segregation measure
            2                                                 //radius of neighborhood
            (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
            Social.ksSegments                                 //list of segments
            st.CA                                             //current state of CA
            segF                                              //function to return segment for each individual
    let dffsn =  Social.diffusion st.CA
    seg,dffsn

let segAt ca fSeg indv = Social.segregationAt                        //Schelling-like segregation measure
                            2                                       //radius of neighborhood
                            (1.0 / float Social.ksSegments.Length)  //proportion of each segment or group at start
                            Social.ksSegments                       //list of segments
                            ca                                      //current state of CA
                            fSeg
                            indv

let dfsnAt ca p = Social.diffusionAt ca p

let statRec rsc i ipdSol st  (config:RunId) primKS =
  //let seg,dffsn = if rsc.CalcSocMetrics then socialMetrics st segF else -1.0, -1.0
  let segF indv = primKS indv.KS |> Social.ksNum
  let iSeg,iDffsn = 
    if rsc.CalcSocMetrics then
        st.CA.Population |> PSeq.map (fun i -> i.Id,segAt st.CA segF i) |> PSeq.toArray |> Array.sortBy fst |> Array.map snd,
        st.CA.Population |> PSeq.map (fun i -> i.Id,dfsnAt st.CA i) |> PSeq.toArray |> Array.sortBy fst |> Array.map snd
    else
        [|-1.0|],
        [|-1.0|]

  let seg,dffsn = 
    if rsc.CalcSocMetrics then
        iSeg   |> Array.average,
        iDffsn |> Array.average
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
    IndvSeg = iSeg
    IndvDfsn = iDffsn
    IndvKs = st.CA.Population |> Array.map(fun i-> primKS i.KS |> Social.ksNum)
  }

let writeRun rsc rst =
  let seg,dffsn =if rsc.CalcSocMetrics then socialMetrics rst.Step rst.PrimKS else -1.0, -1.0
  let ft = match rst.Step.Best with [] -> 0.0 | x::_ -> x.MFitness
  let line = String.Join("\t",rst.Id,rst.A,rst.SampleNum,rst.Landscape,rst.Step.Count,ft,rst.Ws.EnvChangeCount,rst.Ws.M.H,seg,dffsn)
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
    writeRun rsc rst
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



let runConfig rsc fstrCommunity fstrRun  (runId:RunId) =

  //printfn "config %A %A %A" config.A config.Id config.Run
  let ws = createEnv rsc runId.Id runId.A
  let f : Fitness = ref ws.F
  //init pop and belief space
  let wtdBsp = bsp f parmDefs comparator
  let wtdPop = createPop rsc wtdBsp parmDefs CAUtils.baseKsInit
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
  let ipdCA = makeCA f comparator ipdPop ipdBsp ipdKd ipdInf (getNetwork runId.Net)
  let wtdCA = makeCA f comparator wtdPop wtdBsp wtdKd KDWeightedMajority.wtdMajorityInfluence (getNetwork runId.Net)
  let shCA = makeCA f comparator shPop shBsp shKd KDStagHunt.shInfluence (getNetwork runId.Net)
  let stkCA = makeCA f comparator stkPop stkBsp stkKd KDStackelberg.stkInfluence (getNetwork runId.Net)

  let ipdSt =  {CA=ipdCA; Best=[]; Count=0; Progress=[]}
  let ksf = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)

  let wtdSt =  {CA=wtdCA; Best=[]; Count=0; Progress=[]}

  let shSt = {CA=shCA; Best=[]; Count=0; Progress=[]}
  let shKs = (fun (x:Individual<KDStagHunt.ShKnowledge>) -> Social.ksNum (fst x.KS))

  let stkSt = {CA=stkCA; Best=[]; Count=0; Progress=[]}
  let stkKs = (fun (x:Individual<KDStackelberg.StkKnowledge>) -> Social.ksNum (fst x.KS))


  [for i in 1..rsc.NumLandscapes do
    let ws = changeEnv ws  //change landscape using assigned A-value
    ipdCA.Fitness := ws.F  //update fitness functions
    wtdCA.Fitness := ws.F
    stkCA.Fitness := ws.F
    shCA.Fitness := ws.F

    //Note: population is not re-initialized therefore individuals retain locations from prior run
    let ipdRst = {Id= "IPD"; Landscape=i; A=runId.A; PrimKS=Community.gamePrimKs; Ws=ws;
                  EnvCh=true; Step=ipdSt; StrWComm=fstrCommunity; StrWRun=fstrRun; SampleNum=runId.Run}
    let wtdRst = {Id= "WTD"; Landscape=i; A=runId.A; PrimKS=Community.basePrimKs; Ws=ws;
                  EnvCh=true; Step=wtdSt; StrWComm=fstrCommunity; StrWRun=fstrRun; SampleNum=runId.Run}
    let shRst = {Id= "SH"; Landscape=i; A=runId.A; PrimKS=Community.fstPrimKs; Ws=ws;
                  EnvCh=true; Step=shSt; StrWComm=fstrCommunity; StrWRun=fstrRun; SampleNum=runId.Run}
    let stkRst = {Id= "STK"; Landscape=i; A=runId.A; PrimKS=Community.fstPrimKs; Ws=ws;
                  EnvCh=true; Step=stkSt; StrWComm=fstrCommunity; StrWRun=fstrRun; SampleNum=runId.Run}

    for kd in rsc.KDs do
        match kd with
        | WTD -> 
            let wtdSol,wtdSt = runToSol rsc wtdRst |> Async.RunSynchronously 
            yield statRec rsc i wtdSol wtdSt {runId with Id="WTD"} wtdRst.PrimKS
        | IPD -> 
            let ipdSol,ipdSt = runToSol rsc ipdRst |> Async.RunSynchronously
            yield statRec rsc i ipdSol ipdSt {runId with Id="IPD"} ipdRst.PrimKS
        | SH -> 
            let shSol,shSt = runToSol rsc shRst    |> Async.RunSynchronously 
            yield statRec rsc i shSol shSt {runId with Id="SH"} shRst.PrimKS
        | STK ->
            let stkSol,stkSt = runToSol rsc stkRst |> Async.RunSynchronously 
            yield statRec rsc i stkSol stkSt {runId with Id="STK"} stkRst.PrimKS
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
          let runId = {RunId.Run=i; RunId.A=a; RunId.Net=n; RunId.Id=""}
          yield! runConfig rsc fstrCommunity fstrRun runId}

let prepFile rsc fileName = 
    let path = Path.Combine(rsc.SaveFolder,fileName)
    if File.Exists path |> not then 
        use fn = new StreamWriter(File.OpenWrite(path))
        fn.WriteLine("ConfigRun\tId\tLandscapeNum\tA\tGenCount\tMax\tSeg\tDffsn\tNet\tIndvSeg\tIndvDffsn\tIndvKS")

let writeLndcspStats rsc fileName ls =
    let path = Path.Combine(rsc.SaveFolder,fileName)
    use fn = File.AppendText(path)
    let line = 
        sprintf "%d\t%s\t%d\t%f\t%d\t%f\t%f\t%f\t%s" 
            ls.ConfigRun ls.Id ls.LandscapeNum ls.A ls.GenCount ls.Max ls.Seg ls.Dffsn ls.Net
    fn.Write(line)
    fn.Write("\t")
    ls.IndvSeg |> Array.iter (fun x -> fn.Write(x); fn.Write("|"))
    fn.Write("\t")
    ls.IndvDfsn |> Array.iter (fun x -> fn.Write(x); fn.Write("|"))
    fn.Write("\t")
    ls.IndvKs |> Array.iter (fun x -> fn.Write(x); fn.Write("|"))
    fn.WriteLine()

let start rsc =
  initLogging rsc
  async {
    let fileName = "Stats.txt"
    prepFile rsc fileName
    let stats = run rsc |> Seq.iter (writeLndcspStats rsc fileName)
    printfn "Done stats"
  } 
