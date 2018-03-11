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

//let w = createWorld 500 2 (5.,15.) (20., 10.) None None (Some 3.1) |> ref
//let m,f = DF1.landscape !w
let FitnessHlder = ref (fun xs -> 0.)
//let maxCone = ref m
//let envChangedCount = ref 0

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

let startCA = kdIpdCA vmx FitnessHlder comparator parmDefs defaultNetwork
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

type WorldState = {W:World; M:Cone; F:float[]->float; EnvChangeCount:int}

let createEnv a =
  let w = createWorld 200 2 (5.,15.) (20., 10.) None None (Some a) 
  let (c,f) = landscape w
  {W=w; M=c; F=f; EnvChangeCount=0}

let changeEnv ws =
  let w = updateWorld ws.W
  let (c,f) = landscape w
  {W=w; M=c; F=f; EnvChangeCount = ws.EnvChangeCount + 1}

let inline run<'a> (st:TimeStep<'a>,ws) f =
    
    let ws,envCh = 
      if st.Count > 0 && st.Count % 100 = 0 && ws.EnvChangeCount < 4 then
        let ws = changeEnv ws
        FitnessHlder := ws.F
        printf "env changed"
        ws,true
      else 
        ws,false
    let st = step envCh st
    let (bfit,gb) = best st
    let solFound = Array.zip gb ws.M.L |> Seq.forall (fun (a,b) -> abs (a - b) < 0.01)
    if solFound then 
        printfn "sol @ %d - B=%A - C=%A" st.Count (bfit,gb) ws.M
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
    ((segs,dffns),(st,ws))

let inline runNoStats <'a> (st:TimeStep<'a>,ws) =
    let ws,envCh = 
      if st.Count > 0 && st.Count % 100 = 0 && ws.EnvChangeCount < 4 then
        let ws = changeEnv ws
        FitnessHlder := ws.F
        printf "env changed"
        ws,true
      else 
        ws,false
    let st = step envCh st
    let (bfit,gb) = best st
    let solFound = Array.zip gb ws.M.L |> Seq.sumBy (fun (a,b) -> sqr (a - b)) |> sqrt < 0.001
    if solFound then 
        printfn "sol @ %d - B=%A - C=%A" st.Count (bfit,gb) ws.M
    let dffns = Social.diffusion st.CA
    printfn "gen %d" st.Count
    (st,ws)
  
let runStats st ksf =
  let Avals = [3.1; 3.99]
  Avals |> List.map (fun a ->
    let ws = createEnv a
    FitnessHlder := ws.F
    let stats = (st,ws) |> List.unfold (fun (st,ws) -> 
      let (stats,(st,ws)) = run (st,ws) ksf 
      if st.Count > 500 then 
        None 
      else 
        Some (stats,(st,ws)))
    a,stats)

let runIPD() =
  [3.1; 3.99] 
  |> List.collect (fun avalue ->
    [0..30] 
    |> List.map (fun i -> 
      let ws = createEnv avalue
      FitnessHlder := ws.F
      let startCA = kdIpdCA vmx FitnessHlder comparator parmDefs defaultNetwork
      let st = {CA=startCA; Best=[]; Count=0; Progress=[]}
      let st,ws = ((st,ws),[1..500]) ||> List.fold (fun acc i -> runNoStats acc)
      let ksf = (fun (x:Individual<KDIPDGame.IpdKS>) -> Social.ksNum (fst x.KS).KS)
      let maxHop = st.CA.Population.Length / 36 / 2
      let segs = [1..maxHop] |> List.map (fun i -> 
        Social.segregation                                    //Schelling-like segregation measure
            i                                                 //radius of neighborhood
            (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
            Social.ksSegments                                 //list of segments
            st.CA                                             //current state of CA
            ksf                                              //funtion to return segment for each individual
            )
      let dffns = Social.diffusion st.CA
      ("ipd",avalue,i,segs,dffns)))

let runWTD() =
  [3.1; 3.99] 
  |> List.collect (fun avalue ->
    [0..30] 
    |> List.map (fun i -> 
      let ws = createEnv avalue
      FitnessHlder := ws.F
      let startCA = kdWeightedCA FitnessHlder comparator parmDefs defaultNetwork
      let st = {CA=startCA; Best=[]; Count=0; Progress=[]}
      let st,ws = ((st,ws),[1..500]) ||> List.fold (fun acc i -> runNoStats acc)
      let maxHop = st.CA.Population.Length / 36 / 2
      let segs = [1..maxHop] |> List.map (fun i -> 
        Social.segregation                                    //Schelling-like segregation measure
            i                                                 //radius of neighborhood
            (1.0 / float Social.ksSegments.Length)            //proportion of each segment or group at start
            Social.ksSegments                                 //list of segments
            st.CA                                             //current state of CA
            Social.baseSeg                                    //funtion to return segment for each individual
            )
      let dffns = Social.diffusion st.CA
      ("wtd",avalue,i,segs,dffns)))

//let runWtd() =
//  [0..30] |> List.map (fun i -> 
//    let startCA = kdWeightedCA FitnessHlder comparator parmDefs
//    let stats = runStats {CA=startCA; Best=[]; Count=0; Progress=[]} Social.baseSeg
//    (i,stats))

(*


*)

let rIpd = runIPD()
let rWtd = runWTD()

let calcStatsSeg runs =
  let runsG = runs |> List.groupBy (fun (m,a,_,_,_) -> m,a)
  let runGSegs = 
    runsG 
    |> List.map (fun (k,xs) ->
      k,
      xs |> List.map (fun (m,a,idx,sgs,dfs) -> sgs))
  let runsMeanStdv =
      runGSegs
      |> List.map (fun (k,sgs) ->
        let l = List.length sgs |> float
        let means = Array.zeroCreate (List.head sgs |> List.length)
        let means=(means,sgs) ||> List.fold (fun acc sg -> 
          Seq.zip acc sg |> Seq.map (fun (a,b) -> a + b) |> Seq.toArray)
        let means = means |> Array.map (fun a -> a / l)
        let stdvs = Array.zeroCreate (List.head sgs |> List.length)
        let stdvs = (stdvs,sgs) ||> List.fold (fun acc sg -> 
          Seq.zip acc sg |> Seq.mapi (fun i (ac,b) ->  ac + sqr (means.[i] - b)) |> Seq.toArray)
        let stdvs = stdvs |> Array.map (fun s -> s / l |> sqrt)
        k,means,stdvs)
  runsMeanStdv

let calcStatsDfsn runs =
  let runsG = runs |> List.groupBy (fun (m,a,_,_,_) -> m,a)
  let runGSegs = 
    runsG 
    |> List.map (fun (k,xs) ->
      k,
      xs |> List.map (fun (m,a,idx,sgs,dfs) -> dfs))
  let runsMeanStdv =
      runGSegs
      |> List.map (fun (k,dfs) ->
        let l = List.length dfs |> float
        let mean = 0.
        let mean=(mean,dfs) ||> List.fold (+)
        let mean =mean / l
        let stdv=0.
        let stdv=(stdv,dfs)||>List.fold(fun acc df -> acc + sqr (df - mean))
        let stdv = stdv / l |> sqrt
        k,mean,stdv)
  runsMeanStdv

let ipdStats = calcStatsSeg rIpd
let wtdStats = calcStatsSeg rWtd

open FSharp.Charting

let getMeans stats = stats |> Seq.collect (fun ((ca,aval),means,stdvs) -> means |> Seq.mapi (fun i m -> (ca,aval,i+1),m))
let getStdvs stats = stats |> Seq.collect (fun ((ca,aval),means,stdvs) -> stdvs |> Seq.mapi (fun i m -> (ca,aval,i+1),m))

let ipdMeans = getMeans ipdStats |> Seq.toArray
let wtdMeans = getMeans wtdStats |> Seq.toArray

let ipdStdvs = getStdvs ipdStats |> Seq.toArray
let wtdStdvs = getStdvs wtdStats |> Seq.toArray

let ipdDfs = calcStatsDfsn rIpd
let wtdDfs = calcStatsDfsn rWtd

let dumpSegMeans segStats = segStats |> Seq.iter (fun ((ca,aval,run),mean) -> printfn "%s,%0.2f,%d,%f" ca aval run mean)
let dumpSegStdvs segStats = segStats |> Seq.iter (fun ((ca,aval,run),stdv) -> printfn "%s,%0.2f,%d,%f" ca aval run stdv)

dumpSegMeans ipdMeans
dumpSegStdvs ipdStdvs

dumpSegMeans wtdMeans
dumpSegStdvs wtdStdvs

let dumpDfsn stats = stats |> Seq.iter (fun ((ca,aval),mean,stdv)->printfn "%s,%0.2f,%f,%f" ca aval mean stdv)

dumpDfsn ipdDfs
dumpDfsn wtdDfs