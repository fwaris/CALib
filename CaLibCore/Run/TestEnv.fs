﻿module TestEnv
open CA
open CAUtils

//let defaultNetwork = CAUtils.l4BestNetwork
//let defaultNetwork = CAUtils.hexagonNetwork
let defaultNetwork = CAUtils.hexagonNetworkViz

let inline makeCA fitness comparator pop bspace kd influence network =
        {
            Population           = pop
            Network              = network
            KnowlegeDistribution = kd
            BeliefSpace          = bspace
            Acceptance           = CARunner.acceptance 0.25 comparator
            Influence            = influence
            Update               = CARunner.update
            Fitness              = fitness
            Comparator           = comparator
        }

let termination step = step.Count > 1000
let best stp = if stp.Best.Length > 0 then stp.Best.[0].MFitness,stp.Best.[0].MParms else 0.0,[||]

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

let inline bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness

let inline createPop bsp parms init = CAUtils.createPop (init bsp) parms 900 true

//kd construction
//let simpleMajorityKDist  = KD(KDSimpleMajority.knowledgeDist)
let wtdMajorityKdist c p = KD(KDWeightedMajority.knowledgeDist p 8 c)
//let lWtdMajorityKdist  c = KD(KDLocallyWeightedMajority.knowledgeDist c)
//let gameKdist        c p = KDGame.knowledgeDist c KDGame.hawkDoveGame p defaultNetwork
//let hedonicKdist  c p    = KDHedonicGame.knowledgeDist c p defaultNetwork
let ipdKdist explKs ada vmx cmprtr pop  = KDIPDGame.knowledgeDist explKs ada vmx cmprtr pop
let rule1 = Schelling.r1 0.5
let schKdist      c p    = Schelling.knowledgeDist rule1 c p


//CA construction
//let kdSimpleCA   f c p  = 
//    let bsp = bsp f p c
//    let pop = createPop bsp p CAUtils.baseKsInit
//    makeCA f c pop bsp simpleMajorityKDist CARunner.baseInfluence

let kdWeightedCA f c p  = 
    let bsp = bsp f p c
    let ksSet = CAUtils.flatten bsp |> List.map (fun ks->ks.Type) |> set
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (wtdMajorityKdist c ksSet) CARunner.baseInfluence

let kdSchelligCA f c p  = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.baseKsInit
    makeCA f c pop bsp (schKdist c pop) CARunner.baseInfluence

//let kdlWeightedCA f c p = 
//    let bsp = bsp f p c
//    let pop = createPop bsp p CAUtils.baseKsInit
//    makeCA f c pop bsp (lWtdMajorityKdist c) CARunner.baseInfluence

//let kdGame2PlayerCA f c p = 
//    let bsp = bsp f p c
//    let pop = createPop bsp p CAUtils.baseKsInit
//    makeCA f c pop bsp (gameKdist c pop) CARunner.baseInfluence

//let kdHedonicCA f c p = 
//    let bsp = bsp f p c
//    let pop = createPop bsp p CAUtils.ksSetInit
//    let kd = hedonicKdist c pop 
//    makeCA f c pop bsp kd KDHedonicGame.setInfluence

let kdIpdCA vmx f c p  = 
    let bsp = bsp f p c
    let pop = createPop bsp p CAUtils.baseKsInit |> KDIPDGame.initKS
    let ada = KDIPDGame.Geometric(0.9,0.1)
    let kd,inf = ipdKdist Domain ada vmx c pop 
    makeCA f c pop bsp kd inf


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



