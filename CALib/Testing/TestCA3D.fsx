#r @"../../packages/FSharp.Collections.ParallelSeq.1.0.2\lib\net40\FSharp.Collections.ParallelSeq.dll"
#load "../CA.fs"
#load "../CAUtils.fs"
#load "../BeliefSpace/SituationalKS.fs"
#load "../BeliefSpace/NormativeKS.fs"
#load "../BeliefSpace/HistoricalKS.fs"
#load "../BeliefSpace/DomainKS.fs"
#load "../KnowledgeDistribution/KDSimpleMajority.fs"
#load "../KnowledgeDistribution/KDWeightedMajority.fs"
#load "../KnowledgeDistribution/KDGame.fs"
#load "../KnowledgeDistribution/KDLocallyWeightedMajority.fs"
#load "../KnowledgeDistribution/KDHedonicGame.fs"
#load "../CARunner.fs"
open CA

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
let beliefSpace = CARunner.defaultBeliefSpace parms comparator fitness
//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let pop         = CAUtils.createPop parms 1000 beliefSpace true

let gameKdist           = KDGame.knowledgeDist comparator KDGame.hawkDoveGame pop CAUtils.l4BestNetwork
let simpleMajorityKDist = KD(KDSimpleMajority.knowledgeDist)
                  //best game 7.071035596; [(Normative, 1000)]
                  //best majority 7.070912972 seq [(Normative, 1000)]
let wtdMajorityKdist = KD(KDWeightedMajority.knowledgeDist comparator)
                  //best game 7.071035596; [(Normative, 1000)]
                  //best majority 7.070912972 seq [(Normative, 1000)]
let lWtdMajorityKdist = KD(KDLocallyWeightedMajority.knowledgeDist comparator)

let hedonicKdist = KDHedonicGame.knowledgeDist comparator pop CAUtils.l4BestNetwork

let ca =
    {
        Population           = pop
        Network              = CAUtils.l4BestNetwork
        KnowlegeDistribution = simpleMajorityKDist
        BeliefSpace          = beliefSpace
        Acceptance           = CARunner.acceptance 5 comparator
        Influence            = CARunner.influence
        Update               = CARunner.update
        Fitness              = fitness
        Comparator           = comparator
    }

let termination step = step.Count > 1000
let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness else 0.0
let dataCollector s = best s, s.CA.Population |> Seq.collect (fun p->p.KS) |> Seq.countBy CAUtils.yourself |> Seq.sortBy fst |> Seq.toList

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

let kdSimple        = ca |> runCollect dataCollector 2 |> tk
let kdWeigthed      = {ca with KnowlegeDistribution=wtdMajorityKdist} |> runCollect dataCollector 2 |> tk
let kdlWeigthed     = {ca with KnowlegeDistribution=lWtdMajorityKdist} |> runCollect dataCollector 2 |> tk
let kdGame2Player   = {ca with KnowlegeDistribution=gameKdist} |> runCollect dataCollector 2 |> tk
let kdHedonic       = {ca with KnowlegeDistribution=hedonicKdist} |> runCollect dataCollector 2 |> tk
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
    let ldata = (Map.empty,(kd |> Seq.map snd)) ||> Seq.fold (fun m xs -> (m,xs) ||> Seq.fold (fun m (k,v:int) -> add m k v)) |> Map.map (fun k v -> List.rev v)
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


(*
*)
