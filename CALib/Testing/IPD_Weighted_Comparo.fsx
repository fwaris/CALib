#load "TestEnv.fsx"
#load "..\DF1.fs"
#load "ObservableExt.fs"
open CAUtils
open TestEnv
open CA
open DF1
open System.IO

let parms = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

//2d df1 
let fitness df (parms:Parm array)  = 
    let x = match parms.[0] with F(v,_,_) -> v | _ -> failwith "no match"
    let y = match parms.[1] with F(v,_,_) -> v | _ -> failwith "no match"
    df x y

let comparator  = CAUtils.Maximize

//let bsp fitness parms comparator = Roots [ Leaf (DomainKS2.create comparator fitness 2); Leaf (NormativeKS.create parms comparator)]
let bsp fitness parms comparator = CARunner.defaultBeliefSpace parms comparator fitness
let inline createPop bsp parms init size = CAUtils.createPop (init bsp) parms size true

let kdIpdCA ada vmx  f c p size = 
    let b = bsp f p c
    let pop = createPop b p CAUtils.baseKsInit size |> KDIPDGame.initKS
    let ada = KDIPDGame.Geometric(ada)
    let kd = ipdKdist ada vmx c pop 
    makeCA f c pop b kd KDIPDGame.ipdInfluence

let kdWeightedCA f c p size = 
    let bsp = bsp f p c
    let ksSet = CAUtils.flatten bsp |> List.map (fun ks->ks.Type) |> set
    let pop = createPop bsp p CAUtils.baseKsInit size
    makeCA f c pop bsp (wtdMajorityKdist c ksSet) KDWeightedMajority.wtdMajorityInfluence

let rnd = System.Random()
let coneSizes = [100; 500; 1000]
let popSizes = [100; 250; 1000]
let startStep ca = {CA=ca; Best=[]; Count=0; Progress=[]}
let ada = (0.7, 0.1) //(0.9, 0.2)
let vmx = (0.2, 0.9)//(0.85,0.15)
let startCAs() = 
    [
        for c in coneSizes do
            for sz in popSizes do
                let df1,maxCone = DF1.df1_2d rnd c (5.,20.) (10., 40.)
                let fit = fitness df1
                let ipdCA = (sprintf "ipd,%d,%d" c sz),maxCone,kdIpdCA ada vmx fit comparator parms sz |> startStep
                let wtdCA = (sprintf "wtd,%d,%d" c sz),maxCone,kdWeightedCA fit comparator parms sz |> startStep
                yield ipdCA, wtdCA
    ]

type Step<'a> = SolFound of int | Cont of 'a | MaxCountReached

let max_count = 2500

let step (desc,maxCone,st) = 
    let st = CARunner.step st 2
    //printfn "%s %d" desc st.Count
    let (h,(_,_)) = maxCone
    if st.Best.[0].Fitness - h |> abs < 0.001 then SolFound st.Count
    elif st.Count > max_count then MaxCountReached
    else Cont (desc,maxCone,st)

let rec run st =
    match step st with
    | SolFound x -> x
    | MaxCountReached -> max_count
    | Cont st -> run st

let hyperPSearch() =
    let grid =  [for g in 0.9 .. -0.1 .. 0.5 do for m in 0.5 .. -0.1 .. 0.1 do yield g,m]
    let runAda ada = 
        let scapes = 
            [
                for c in coneSizes do
                    for sz in popSizes do
                        let df1,maxCone = DF1.df1_2d rnd c (5.,20.) (10., 40.)
                        let fit = fitness df1
                        let ipdCA = (sprintf "ipd,%d,%d" c sz),maxCone,kdIpdCA ada vmx fit comparator parms sz |> startStep
                        yield ipdCA
            ]
        let runs =
            [for i in 1 .. 5 -> i] 
            |> List.collect(fun _ ->
                scapes 
                |> List.map (fun scape ->
                    let (s,_,_) = scape
                    let (dipd,_,_) = scape
                    let ripd = run scape
                    printfn "%A" (ada,dipd,ripd)
                    dipd,ripd))
        ada,runs
    let g = grid |> List.map runAda  
    let gg = g |> List.groupBy fst |> List.map (fun (x,xs)->x,xs|>List.sumBy (fun (x,ys) -> ys|> List.sumBy snd))
    let gmin = gg |> List.sortBy snd
    //best 3 =   [((0.6, 0.1), 10918); ((0.7, 0.1), 13963); ((0.6, 0.3), 14504);
    g

let data = 
    [for i in 1 .. 50 -> i] 
    |> List.collect (fun _ -> 
        startCAs() |> List.map (fun (ipd,wtd) -> 
            let (dipd,_,_) = ipd
            let (dwtd,_,_) = wtd
            let ripd = run ipd
            let rwtd = run wtd
            printfn "%s %d, %s %d" dipd ripd dwtd rwtd
            (dipd,ripd),(dwtd,rwtd)
            ))
    
let ipd = data |> List.map fst |> List.map (fun (a,b) ->a, float b)
let wtd = data |> List.map snd |> List.map (fun (a,b) ->a, float b)

let gipd = ipd |> List.groupBy fst
let gwtd = wtd |> List.groupBy fst

let ipdMeans = gipd |> List.map (fun (g,xs) -> g, xs |> List.averageBy snd) |> Map.ofList
let wtdMeans = gwtd |> List.map (fun (g,xs) -> g, xs |> List.averageBy snd) |> Map.ofList

let sqr x = x * x
let sigma mu xs = (xs |> List.sumBy (fun (_,x) -> sqr (mu - x))) / float xs.Length |> sqrt

let ipdSigmas = gipd |> List.map (fun (g,xs)->let mu = ipdMeans.[g] in g,mu, sigma mu xs)
let wtdSigmas = gwtd |> List.map (fun (g,xs)->let mu = wtdMeans.[g] in g,mu, sigma mu xs)
;;
ipdSigmas |> List.iter (fun (a,b,c)  -> printfn "%s;%f;%f" a b c);;
wtdSigmas |> List.iter (fun (a,b,c)  -> printfn "%s;%f;%f" a b c);;
;;
ipd |> List.iter (fun (a,b) -> printfn "%s; %f" a b);;
wtd |> List.iter (fun (a,b) -> printfn "%s; %f" a b);;

let dmp = List.iter (fun (a,b) -> printfn "%s;%f" a b)
for (g,xs) in  gipd do printfn "*%s" g; dmp xs
for (g,xs) in  gwtd do printfn "*%s" g; dmp xs


let gdata = data |> List.groupBy (fst>>fst)
for (g,xs) in gdata do printfn "*%s" g; xs |> List.iter (fun ((_,i),(_,w)) -> printfn "%d;%d" i w)

Seq.zip ipdMeans wtdMeans |> Seq.iter (printfn "%A")
Seq.zip ipdSigmas wtdSigmas |> Seq.iter (printfn "%A")


(*
ada = (0.9,02)
([ipd,100,100, 691.94], [wtd,100,100, 1225.72])
([ipd,100,1000, 21.38], [wtd,100,1000, 203.72])
([ipd,100,250, 229.56], [wtd,100,250, 455.9])
([ipd,1000,100, 1253.94], [wtd,1000,100, 1624.16])
([ipd,1000,1000, 803.62], [wtd,1000,1000, 896.52])
([ipd,1000,250, 1103], [wtd,1000,250, 1090.74])
([ipd,500,100, 848.5], [wtd,500,100, 1637.18])
([ipd,500,1000, 472.26], [wtd,500,1000, 494.04])
([ipd,500,250, 823.12], [wtd,500,250, 1102.96])
val it : unit = ()

> 
(("ipd,100,100", 691.94, 984.4655486), ("wtd,100,100", 1225.72, 1077.161604))
(("ipd,100,250", 229.56, 522.283186), ("wtd,100,250", 455.9, 871.6661345))
(("ipd,100,1000", 21.38, 43.11792667), ("wtd,100,1000", 203.72, 530.2306683))
(("ipd,500,100", 848.5, 1026.950383), ("wtd,500,100", 1637.18, 1020.855145))
(("ipd,500,250", 823.12, 1025.703498), ("wtd,500,250", 1102.96, 1091.053234))
(("ipd,500,1000", 472.26, 838.530782), ("wtd,500,1000", 494.04, 835.3013099))
(("ipd,1000,100", 1253.94, 1120.275313), ("wtd,1000,100", 1624.16, 1093.66747))
(("ipd,1000,250", 1103.0, 1143.728167), ("wtd,1000,250", 1090.74, 1086.001893))
(("ipd,1000,1000", 803.62, 1032.896566), ("wtd,1000,1000", 896.52, 1052.432634))
val it : unit = ()

*)
