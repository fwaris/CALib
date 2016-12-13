#r @"../../packages/FSharp.Collections.ParallelSeq\lib\net40\FSharp.Collections.ParallelSeq.dll"
#load "../CA.fs"
#load "../Probability.fs"
#load "../CAUtils.fs"
#load "../CAEvolve.fs"
#load "../BeliefSpace/SituationalKS.fs"
#load "../BeliefSpace/NormativeKS.fs"
#load "../BeliefSpace/HistoricalKS.fs"
#load "../BeliefSpace/DomainKS2.fs"
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

//let defaultNetwork = CAUtils.l4BestNetwork
let defaultNetwork = CAUtils.hexagonNetwork

let inline makeCA fitness comparator pop bspace kd influence =
        {
            Population           = pop
            Network              = defaultNetwork
            KnowlegeDistribution = kd
            BeliefSpace          = bspace
            Acceptance           = CARunner.acceptance 10 comparator
            Influence            = influence
            Update               = CARunner.update
            Fitness              = fitness
            Comparator           = comparator
        }

let termination step = step.Count > 1000
let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness,stp.Best.[0].Parms else 0.0,[||]

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
//        printfn "step %i. fitness=%A" stp.Count (best stp)
//        printfn "KS = %A" (stp.CA.Population |> Seq.countBy (fun x->x.KS))
        stp
    let step = {CA=ca; Best=[]; Count=0; Progress=[]}
    step 
    |> Seq.unfold (fun s -> let s = loop s in (data s,s)  |> Some ) 

let inline bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness

let inline createPop bsp parms init = CAUtils.createPop (init bsp) parms 1000 true

//kd construction
let simpleMajorityKDist  = KD(KDSimpleMajority.knowledgeDist)
let wtdMajorityKdist c p = KD(KDWeightedMajority.knowledgeDist p 3 c)
let lWtdMajorityKdist  c = KD(KDLocallyWeightedMajority.knowledgeDist c)
let gameKdist        c p = KDGame.knowledgeDist c KDGame.hawkDoveGame p defaultNetwork
let hedonicKdist  c p    = KDHedonicGame.knowledgeDist c p defaultNetwork
let ipdKdist      c p    = KDIPDGame.knowledgeDist c p


//CA construction
let kdSimpleCA   f c p  = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp simpleMajorityKDist CARunner.baseInfluence

let kdWeightedCA f c p  = 
    let bsp = bsp f p c
    let ksSet = CAUtils.flatten bsp |> List.map (fun ks->ks.Type) |> set
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (wtdMajorityKdist c ksSet) CARunner.baseInfluence

let kdlWeightedCA f c p = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (lWtdMajorityKdist c) CARunner.baseInfluence

let kdGame2PlayerCA f c p = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (gameKdist c pop) CARunner.baseInfluence

let kdHedonicCA f c p = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.ksSetInit
    let kd = hedonicKdist c pop 
    makeCA f c pop bsp kd KDHedonicGame.setInfluence

let kdIpdCA vmx f c p  = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.baseKsInit |> KDIPDGame.initKS
    let kd = ipdKdist vmx c pop 
    makeCA f c pop bsp kd KDIPDGame.ipdInfluence

#r @"..\..\packages\FSharp.Charting\lib\net40\FSharp.Charting.dll"
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
let ks = function Domain -> "Domain" | Historical -> "Historical" | Situational -> "Situational" | Normative -> "Normative" | _ -> failwith "shoud not happen"

let withBackground image (c:FSharp.Charting.ChartTypes.GenericChart) = 
    c.ApplyToChart (fun c -> 
        let a = c.ChartAreas.[0]
        a.BackImage <- image
        a.BackImageWrapMode <- System.Windows.Forms.DataVisualization.Charting.ChartImageWrapMode.Scaled)

let plotResults title kd =
    let lbls = labels (kd |> Seq.map snd)
    let cls = [for i in 0 .. lbls |> Seq.length -> toColor(colors.[i])]
    let ldata = (Map.empty,(kd |> Seq.map snd)) ||> Seq.fold (fun m xs -> (m,xs) ||> Seq.fold (fun m (k,v:float) -> add m k v)) |> Map.map (fun k v -> List.rev v)
    let maxF = kd |> Seq.map (fst>>fst) |> Seq.max
    lbls 
    |> Seq.mapi (fun i l -> Chart.Line (ldata.[l], Name=ks l)) 
    |> Seq.append 
        [Chart.Line (
            kd 
            |> Seq.map (fst>>fst)
            |> Seq.map  (fun x -> x * 80.), Name=sprintf "Fitness [%f] (scaled)" maxF)             
        |> Chart.WithSeries.Marker(Style=System.Windows.Forms.DataVisualization.Charting.MarkerStyle.Diamond)
       ]
    |> Chart.Combine 
    |> Chart.WithLegend(Enabled=true) 
    |> Chart.WithTitle(Text=title)
    |> Chart.WithArea.AxisX(Title="Generation")




