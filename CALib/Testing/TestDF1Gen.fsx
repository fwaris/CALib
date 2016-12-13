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

let termination d maxCone step = 
    let (f,_) = best step 
    let c1 = step.Count > 2500 
    let c2 = abs (f - maxCone.H) < 0.01
    if c1 then printfn "no solution %s - tg %A" d maxCone
    if c2 then printfn "sol %s %d" d step.Count
    c1 || c2


let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness else 0.0
let tk s = s |> Seq.truncate 100 |> Seq.toList

(* vmin,vmax hyperparameter search -> .7, 1.4 ; .5,1.9
*)

let runT  vmx (l,m,f) = 
    let l = "ipd " + l
    let t = kdIpdCA vmx f comparator parms |> CARunner.run l (termination l m) 2
    l,m,best t

let ipdsT vmx = fits |> List.map (runT vmx)

//let ipds = ipdsT  (0.2, 0.9)

let runW (l,m,f) = 
    let l = "wtd " + l
    let t = kdWeightedCA f comparator parms |> CARunner.run l (termination l m) 2
    l,m,best t

//let wtdsT = fits |> List.map runW

let stats =
    seq {
        for i in 1 .. 100 do
        let ipds = ipdsT (0.2, 0.9)
        let wtdsT = fits |> List.map runW
        yield! (ipds |> List.map (fun (l,m,f) -> "ipd",l,m,f))
        yield! (wtdsT |> List.map (fun (l,m,f) -> "wtd",l,m,f))
        }
    |> Seq.toList

let scores = stats |> List.map (fun (a,b,_,c) -> a,b,c)
let wtd = scores |> List.filter (fun (a,b,c)->a="wtd")
let ipd = scores |> List.filter (fun (a,b,c)->a="ipd")
let wtdg = wtd |> List.groupBy (fun (a,b,c)->b) 
let ipdg = ipd |> List.groupBy (fun (a,b,c)->b)
let wtda = wtdg |> List.map (fun (g,xs) -> g, xs |> List.averageBy (fun (a,b,c) -> c))
let ipda = ipdg |> List.map (fun (g,xs) -> g, xs |> List.averageBy (fun (a,b,c) -> c));;
(*

*)
