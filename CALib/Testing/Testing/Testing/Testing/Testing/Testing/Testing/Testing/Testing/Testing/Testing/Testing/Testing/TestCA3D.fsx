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
let fitFunc (parms:float array) = 
    let x = parms.[0] 
    let y = parms.[1] 
    let z = parms.[2]
    let c1 = x**2. - y**2. + z**2. <= 2.
    let c2 = x**2. + y**2. + z**2. <= 10.
    if c1 && c2 then
        x*y + y*z
    else
        -9999.0

let comparator  = CAUtils.Maximize

let fitness = ref fitFunc

let termination step = step.Count > 1000
let best stp = if stp.Best.Length > 0 then stp.Best.[0].MFitness else 0.0

let tk s = s |> Seq.take 50 |> Seq.toList

let kdWeightedCA    = kdWeightedCA (basePop parms fitness) parms fitness

let kdIpdCA         = kdIpdCA (basePop parms fitness) parms fitness

let kdWeigthed      = kdWeightedCA |> runCollect dataCollector 2 |> tk
let kdIpd           = kdIpdCA |> runCollect ipdDataCollector 2 |> tk
//

plotResults "Weigted Majority" kdWeigthed
plotResults "IPD Game" kdIpd


(*
*)
