#load "TestEnv.fsx"
open TestEnv
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

let termination step = step.Count > 1000
let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness else 0.0

let tk s = s |> Seq.take 50 |> Seq.toList

let kdSimpleCA      = kdSimpleCA fitness comparator parms
let kdWeightedCA    = kdWeightedCA fitness comparator parms
let kdlWeightedCA   = kdlWeightedCA fitness comparator parms
let kdGame2PlayerCA = kdGame2PlayerCA fitness comparator parms
let kdHedonicCA     = kdHedonicCA fitness comparator parms
let kdIpdCA         = kdIpdCA fitness comparator parms

let kdSimple        = kdSimpleCA |> runCollect dataCollector 2 |> tk
let kdWeigthed      = kdWeightedCA |> runCollect dataCollector 2 |> tk
let kdlWeigthed     = kdlWeightedCA |> runCollect dataCollector 2 |> tk
let kdGame2Player   = kdGame2PlayerCA |> runCollect dataCollector 2 |> tk
let kdHedonic       = kdHedonicCA |> runCollect setDataClctr 2 |> tk
let kdIpd           = kdIpdCA |> runCollect ipdDataCollector 2 |> tk
//

plotResults "Simple Majority" kdSimple
plotResults "Weigted Majority" kdWeigthed
plotResults "Locally Weigted Majority" kdlWeigthed
plotResults "Hawk-Dove" kdGame2Player
plotResults "Hedonic Game" kdHedonic
plotResults "IPD Game" kdIpd


(*
*)
