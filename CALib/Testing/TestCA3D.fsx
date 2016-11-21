#r @"../../packages/FSharp.Collections.ParallelSeq.1.0.2\lib\net40\FSharp.Collections.ParallelSeq.dll"
#load "../CA.fs"
#load "../CAUtils.fs"
#load "../CAEvolve.fs"
#load "../BeliefSpace/SituationalKS.fs"
#load "../BeliefSpace/NormativeKS.fs"
#load "../BeliefSpace/HistoricalKS.fs"
#load "../BeliefSpace/DomainKS.fs"
#load "../KnowledgeDistribution/KDSimpleMajority.fs"
#load "../KnowledgeDistribution/KDWeightedMajority.fs"
#load "../KnowledgeDistribution/KDGame.fs"
#load "../KnowledgeDistribution/KDLocallyWeightedMajority.fs"
#load "../KnowledgeDistribution/KDHedonicGame.fs"
#load "../KnowledgeDistribution/KDContinousStrategyGame.fs"
#load "../KnowledgeDistribution/KDIpDGame.fs"
#load "../CARunner.fs"
open CA
open CAUtils

let parms = 
    [|
        F(0.,-0.,10.) // x
        F(0.,-0.,10.) // y
        F(0.,-0.,10.) // z
    |]

//http://en.wikipedia.org/wiki/Nonlinear_programming
// maximize xy + yz
// st. x^2 - y^2 + z^2 <= 2
//     x^2 + y^2 + z^2 <= 10
let fitness (parms:Parm array) = 
    let x = match parms.[0] with F(v,_,_) -> v | _ -> failwith "no match"
    let y = match parms.[1] with F(v,_,_) -> v | _ -> failwith "no match"
    let z = match parms.[2] with F(v,_,_) -> v | _ -> failwith "no match"
    let c1 = x**2. - y**2. + z**2. <= 2.
    let c2 = x**2. + y**2. + z**2. <= 10.
    if c1 && c2 then
        x*y + y*z
    else
        -9999.0

let comparator  = CAUtils.Maximize

let inline makeCA pop bspace kd influence =
        {
            Population           = pop
            Network              = CAUtils.l4BestNetwork
            KnowlegeDistribution = kd
            BeliefSpace          = bspace
            Acceptance           = CARunner.acceptance 5 comparator
            Influence            = influence
            Update               = CARunner.update
            Fitness              = fitness
            Comparator           = comparator
        }

let termination step = step.Count > 1000
let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness else 0.0

let dataCollector s = 
    best s, 
    s.CA.Population 
    |> Seq.map (fun p->p.KS) 
    |> Seq.countBy yourself 
    |> Seq.map (fun (k,i)-> k,float i)
    |> Seq.sortBy fst 
    |> Seq.toList

let setDataClctr s = 
    best s, 
    s.CA.Population 
    |> Seq.collect (fun p->p.KS) 
    |> Seq.countBy yourself 
    |> Seq.map (fun (k,i)-> k,float i)
    |> Seq.sortBy fst 
    |> Seq.toList

let ipdDataCollector s = 
    best s, 
    s.CA.Population 
    |> Seq.collect (fun p-> 
        let (k,m) = p.KS
        ([k,1.0],m) ||> Map.fold (fun acc k v -> (k,v)::acc)
        ) 
    |> Seq.groupBy fst
    |> Seq.map (fun (k,vs) -> k, vs |> Seq.map snd |> Seq.sum)
    |> Seq.sortBy fst
    |> Seq.toList

let runCollect data maxBest ca =
    let loop stp = 
        let stp = CARunner.step stp maxBest
        printfn "step %i. fitness=%A" stp.Count (best stp)
        printfn "KS = %A" (stp.CA.Population |> Seq.countBy (fun x->x.KS))
        stp
    let step = {CA=ca; Best=[]; Count=0; Progress=[]}
    step 
    |> Seq.unfold (fun s -> let s = loop s in (data s,s)  |> Some ) 

let tk s = s |> Seq.take 50 |> Seq.toList

let bsp = CARunner.defaultBeliefSpace parms comparator fitness
let bspS = CARunner.defaultBeliefSpace parms comparator fitness
let bspI = CARunner.defaultBeliefSpace parms comparator fitness

//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let popB = CAUtils.createPop (CAUtils.baseKsInit bsp) parms 1000 true
let popS = CAUtils.createPop (CAUtils.ksSetInit bspS) parms 1000 true
let popI = CAUtils.createPop (CAUtils.baseKsInit bsp) parms 1000 true |> KDIPDGame.initKS

let simpleMajorityKDist = KD(KDSimpleMajority.knowledgeDist)
let wtdMajorityKdist    = KD(KDWeightedMajority.knowledgeDist comparator)
let lWtdMajorityKdist   = KD(KDLocallyWeightedMajority.knowledgeDist comparator)
let gameKdist           = KDGame.knowledgeDist comparator KDGame.hawkDoveGame popS CAUtils.l4BestNetwork
let hedonicKdist        = KDHedonicGame.knowledgeDist comparator popS CAUtils.l4BestNetwork
let ipdKdist            = KDIPDGame.knowledgeDist comparator popI

let kdSimpleCA      = makeCA popB bsp simpleMajorityKDist CARunner.baseInfluence
let kdWeightedCA    = makeCA popB bsp wtdMajorityKdist CARunner.baseInfluence
let kdlWeightedCA   = makeCA popB bsp lWtdMajorityKdist CARunner.baseInfluence
let kdGame2PlayerCA = makeCA popB bsp gameKdist CARunner.baseInfluence
let kdHedonicCA     = makeCA popS bspS hedonicKdist KDHedonicGame.setInfluence
let kdIpdCA         = makeCA popI bspI ipdKdist KDIPDGame.ipdInfluence

let kdSimple        = kdSimpleCA |> runCollect dataCollector 2 |> tk
let kdWeigthed      = kdWeightedCA |> runCollect dataCollector 2 |> tk
let kdlWeigthed     = kdlWeightedCA |> runCollect dataCollector 2 |> tk
let kdGame2Player   = kdGame2PlayerCA |> runCollect dataCollector 2 |> tk
let kdHedonic       = kdHedonicCA |> runCollect setDataClctr 2 |> tk
let kdIpd           = kdIpdCA |> runCollect ipdDataCollector 2 |> tk
//
#r @"..\..\packages\FSharp.Charting.0.90.14\lib\net40\FSharp.Charting.dll"
#r "System.Windows.Forms.DataVisualization"
open FSharp.Charting
fsi.AddPrinter(fun (ch:FSharp.Charting.ChartTypes.GenericChart) -> ch.ShowChart() |> ignore; "(Chart)")

let add m k v = m |> Map.add k ( match m |> Map.tryFind k with Some vs -> v::vs | _ -> [v])
let labels s = s |> Seq.collect (fun xs -> xs |> Seq.map fst) |> Seq.distinct

let colors = 
        [
        255,255,0
        255,255,224
        255,250,205
        250,250,210
        255,239,213
        255,228,181
        255,218,185
        238,232,170
        240,230,140
        189,183,107
        255,215,0
        ]

let toColor (r,g,b) = System.Drawing.Color.FromArgb(1,r,g,b)
let ks = function Domain -> "Domain" | Historical -> "Historical" | Situational -> "Situational" | Normative -> "Normative"

let plotResults title kd =
    let lbls = labels (kd |> Seq.map snd)
    let cls = [for i in 0 .. lbls |> Seq.length -> toColor(colors.[i])]
    let ldata = (Map.empty,(kd |> Seq.map snd)) ||> Seq.fold (fun m xs -> (m,xs) ||> Seq.fold (fun m (k,v:float) -> add m k v)) |> Map.map (fun k v -> List.rev v)
    lbls 
    |> Seq.mapi (fun i l -> Chart.Line (ldata.[l], Name=ks l)) 
    |> Seq.append [Chart.Line (kd |> Seq.map fst |> Seq.map  (fun x -> x * 80.), Name="Fitness (scaled)") |> Chart.WithSeries.Marker(Style=System.Windows.Forms.DataVisualization.Charting.MarkerStyle.Diamond)]
    |> Chart.Combine 
    |> Chart.WithLegend(Enabled=true) 
    |> Chart.WithTitle(Text=title)
    |> Chart.WithArea.AxisX(Title="Generation")
//    |> Chart.Show

plotResults "Simple Majority" kdSimple
plotResults "Weigted Majority" kdWeigthed
plotResults "Locally Weigted Majority" kdlWeigthed
plotResults "Hawk-Dove" kdGame2Player
plotResults "Hedonic Game" kdHedonic
plotResults "IPD Game" kdIpd


(*
*)
