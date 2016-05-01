#load "../CA.fs"
#load "../CAUtils.fs"

open CA
open CAUtils
let rec dummyKS = {Type=Situational; Accept=(fun xs -> xs,dummyKS); Influence=fun i -> i}
let testParms =
    [|
        F(10.,10.,100.)
        F32(10.f,10.f,100.f)
        I(10,10,100)
        I64(10L,10L,100L)
    |]

let ind0 = {Id=0; Parms=testParms; Fitness=0.; KS=Situational}
let ind1 = {Id=1; Parms=testParms |> Array.map randomize; Fitness=0.; KS=Situational}

let evolved = ind1.Parms |> Array.map evolveS
if (evolved,ind1.Parms) ||> Array.exists2 (fun a b -> a = b) then
    failwith "evolveS"

let ind01 = influenceInd ind0 ind1
if (ind0.Parms,ind01.Parms) ||> Array.exists2 (fun a b -> a > b) then
    failwith "influenceInd"

let ind1_up = ind1.Parms |> Array.map slideUp
if (ind1.Parms,ind1_up) ||> Array.exists2 (fun a b -> a > b) then
    failwith "slideUp"

let ind1_down = ind1.Parms |> Array.map slideDown
if (ind1.Parms,ind1_down) ||> Array.exists2 (fun a b -> a < b) then
    failwith "slideDown"