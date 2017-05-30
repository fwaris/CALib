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

let kdIpdCA vmx f c p size = 
    let b = bsp f p c
    let pop = createPop b p CAUtils.baseKsInit size |> KDIPDGame.initKS
    let ada = KDIPDGame.Geometric(0.9,0.1)
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
let startCAs() = 
    [
        for c in coneSizes do
            for sz in popSizes do
                let df1,maxCone = DF1.df1_2d rnd c (5.,20.) (10., 40.)
                let fit = fitness df1
                let ipdCA = (sprintf "ipd,%d,%d" c sz),maxCone,kdIpdCA (0.2, 0.9) fit comparator parms sz |> startStep
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
