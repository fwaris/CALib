#load "SetupEnv.fsx"
open CA
open CAUtils

//let defaultNetwork = CAUtils.l4BestNetwork
//let defaultNetwork = CAUtils.hexagonNetwork
let defaultNetwork = CAUtils.hexagonNetworkViz
let defaultComparator = CAUtils.Maximize
let POP_SIZE = 36

let inline makeCA fitness comparator pop bspace influence network =
        {
            Population           = pop
            Network              = network
            BeliefSpace          = bspace
            Acceptance           = CARunner.acceptance 0.25 comparator
            Influence            = influence
            Update               = CARunner.update
            Fitness              = fitness
            Comparator           = comparator
        }

let termination step = step.Count > 1000
let best stp = if stp.Best.Length > 0 then stp.Best.[0].MFitness,stp.Best.[0].MParms else 0.0,[||]
let segF primKS indv  = primKS indv.KS |> Social.ksNum

let basePop parmDefs fitness = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator fitness
    CAUtils.createPop (baseKsInit bsp) parmDefs POP_SIZE true

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
        let ({KDIPDGame.KS=ks;KDIPDGame.Level=lvl},m) = p.KS
        ([ks,lvl],m) ||> Map.fold (fun acc k v -> (k,v)::acc)
        ) 
    |> Seq.groupBy fst
    |> Seq.map (fun (k,vs) -> k, vs |> Seq.map snd |> Seq.sum)
    |> Seq.sortBy fst
    |> Seq.toList

let runCollect data maxBest ca =
    let loop stp = 
        let stp = CARunner.step false stp maxBest
//        printfn "step %i. fitness=%A" stp.Count (best stp)
//        printfn "KS = %A" (stp.CA.Population |> Seq.countBy (fun x->x.KS))
        stp
    let step = {CA=ca; Best=[]; Count=0; Progress=[]}
    step 
    |> Seq.unfold (fun s -> let s = loop s in (data s,s)  |> Some ) 

let kdWeightedCA basePop parmDefs fitness = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator fitness
    let pop = basePop |> Array.map (fun (i:Individual<Knowledge>)-> {i with Parms=Array.copy i.Parms })
    let influence = KDWeightedMajority.influence bsp 3
    makeCA fitness defaultComparator pop bsp influence defaultNetwork

let kdSchelligCA basePop parmDefs fitness  = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator fitness
    let pop = basePop |> Array.map (fun (i:Individual<Knowledge>)-> {i with Parms=Array.copy i.Parms })
    let rule1 = Schelling.r1 0.5
    let influence = Schelling.influence rule1
    makeCA fitness defaultComparator pop bsp influence defaultNetwork

let kdIpdCA basePop parmDefs fitness  = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator fitness
    let pop = basePop |> KDIPDGame.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = 
        let ada = KDIPDGame.Geometric(0.9,0.01)
        let vmx = (0.2, 0.9)
        KDIPDGame.influence Domain ada vmx defaultComparator pop
    makeCA fitness defaultComparator pop bsp influence defaultNetwork

let shCA basePop parmDefs fitness = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator fitness
    let pop = basePop |> KDStagHunt.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStagHunt.influence defaultComparator 5 bsp pop
    makeCA fitness defaultComparator pop bsp influence defaultNetwork

let kdStkCA basePop parmDefs fitness  = 
    let bsp = CARunner.defaultBeliefSpace parmDefs defaultComparator fitness
    let pop = basePop |> KDStackelberg.initKS |> Array.map (fun i -> {i with Parms=Array.copy i.Parms })
    let influence = KDStackelberg.influence defaultComparator pop
    makeCA fitness defaultComparator pop bsp influence defaultNetwork

    
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
let ks = function 
    | Domain        -> "Domain" 
    | Historical    -> "Historical" 
    | Situational   -> "Situational" 
    | Normative     -> "Normative" 
    | Topgraphical  -> "Topographical"
    | _             -> failwith "shoud not happen"

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



