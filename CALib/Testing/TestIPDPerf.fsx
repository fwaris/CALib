#load "TestEnv.fsx"
#load "..\DF1.fs"
open TestEnv
open CA
open CAUtils
open DF1

let parms = 
    [|
        F(0.,-1.,1.) // x
        F(0.,-1.,1.) // y
    |]

let landscapes = [
    "1.01", @"../../Landscapes/test_cone1.01.csv"
    "2.0", @"../../Landscapes/test_cone2.0.csv"
    "3.35", @"../../Landscapes/test_cone3.35.csv"
    "3.5", @"../../Landscapes/test_cone3.5.csv"
    "3.99", @"../../Landscapes/test_cone3.99.csv"
    ]

//2d df1 
let fitness df (parms:Parm array)  = 
    let x = match parms.[0] with F(v,_,_) -> v | _ -> failwith "no match"
    let y = match parms.[1] with F(v,_,_) -> v | _ -> failwith "no match"
    df x y

let fits = landscapes |> List.map (fun (l,f)-> let m,d = createDf1 (__SOURCE_DIRECTORY__ + f) in l,m,fitness d)

let comparator  = CAUtils.Maximize
let termination step = step.Count > 100
let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness else 0.0
let tk s = s |> Seq.truncate 100 |> Seq.toList

(* vmin,vmax hyperparameter search -> .7, 1.4 ; .5,1.9

let runT vmx (l,m,f) = 
    let t = kdIpdCA vmx f comparator parms |> CARunner.run termination 2
    l,m,best t

let ipdsT vmx = fits |> List.map (runT vmx)

let avgs = 
    [for mn in 0.1 .. 0.1 .. 1.0 do
        for mx in 0.2 .. 0.1 .. 1.99 do
            if mx > mn + 0.2 then
                let vmx = (mn,mx)
                let rs = [for _ in 1 .. 5 -> ipdsT vmx]
                let rs = 
                    rs 
                    |> List.collect CAUtils.yourself
                    |> List.groupBy (fun (t,_,_) -> t) 
                    |> List.map (fun (t,xs) -> t,vmx, xs |> List.averageBy (fun (_,_,f) -> f))
                yield rs]

let maxs = 
    avgs
    |> List.collect CAUtils.yourself 
    |> List.groupBy (fun (x,y,z) -> y) 
    |> List.map (fun (x,xs)->x, xs |> List.sumBy(fun (x,y,z)->z))

let maxAll = maxs |> List.maxBy snd
let ms = maxs |> List.sortBy (fun (v,m) -> -m)

maxs |> List.map fst |> List.distinct

*)

(* plot all landscapes

let runC vmx (l,m,f) = 
    let d = kdIpdCA vmx f comparator parms |> runCollect ipdDataCollector 2 |> tk
    l,m,d

let ipdsC vmx = fits |> List.map (runC vmx)
let vmx = (0.7,1.4)
let ipdvmx = ipdsC vmx
for (l,m,d) in ipdvmx do
    async{
        let t = sprintf "%s [%f]" l m.H
        plotResults t d |> FSharp.Charting.Chart.Show
        } |> Async.Start
;;
for (l,m,d) in ipdvmx do
    let f,p = d |> List.last |> fst
    printfn "%s: F=%f,F'=%f, P=%A, P'=%A" l f m.H p [|m.X,m.Y|]
;;

*)

let runT vmx (l,m,f) = 
    let t = kdIpdCA vmx f comparator parms |> CARunner.run termination 2
    l,m,best t

let ipdsT vmx = fits |> List.map (runT vmx)

let vmx = (0.7,1.4)
let ipdruns = [for _ in 1 .. 30 -> ipdsT vmx]
let rs = 
    ipdruns 
    |> List.collect CAUtils.yourself
    |> List.groupBy (fun (t,_,_) -> t) 
    |> List.map (fun (t,xs) -> t,vmx, xs |> List.averageBy (fun (_,_,f) -> f))
let sumRs = rs |> List.sumBy (fun (_,_,f) -> f)


(* averae over  5 runs
coop = d * attraction
.6,1.4 -> 99.43641464
.7,1.4 -> 99.47308799
.65,1.4 -> 99.3658414
.7,1.35 -> 99.4491

.7,1.4 
coop = d + attraction -> 99.3884387
coop =  attraction/d ->  99.43208188
coop =  attraction -> 99.4253216
coop = d * 0.5 * attraction -> 99.33046583
coop = d -> 99.37909667
coop = d/attraction -> 99.35757437
coop = attraction/ (d * 0.5) -> 99.43962729 / 99.30397174 / 99.44627642
coop = attraction/ (d * 1.5) -> 99.36950399
coop = attraction/ (d * 0.4) -> 99.46012299 / 99.38281922 /99.40703995 / 99.44406658
coop = attraction/ (d * 0.3) -> 99.43860639 / 99.43350058 / 99.38714745
coop = attraction/ (d * 0.45) -> 99.33929704
coop = attraction/ (d * 0.35) -> 99.35648942

coop = attraction/ (d * 0.4)
.5,1.9 -> 99.45051976
.7,1.4 -> 99.3987008


 *)