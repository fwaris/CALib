#load "../CA.fs"
#load "../Probability.fs"
#load "../CAUtils.fs"
#load "../CAEvolve.fs"

open CA
open CAUtils
open CAEvolve
open CA

let rec dummyKS = {Type=Situational; Accept=(fun xs -> xs,dummyKS); Influence=fun _ i-> i}
let testParms =
    [|
        F(10.,10.,100.)
        F32(10.f,10.f,100.f)
        I(10,10,100)
        I64(10L,10L,100L)
    |]

let ind0 = {Id=0; Parms=testParms; Fitness=0.; KS=Situational}
let ind1 = {Id=1; Parms=testParms |> Array.map randomize; Fitness=0.; KS=Situational}

let evolved = ind1.Parms |> Array.map (evolveS 1. 1.)
if (evolved,ind1.Parms) ||> Array.exists2 (fun a b -> a = b) then
    failwith "evolveS"

let ind01 = influenceInd 1. 1. ind0 ind1
if (ind0.Parms,ind01.Parms) ||> Array.exists2 (fun a b -> a > b) then
    failwith "influenceInd"

let ind1_up = ind1.Parms |> Array.map (slideUp 1.0)
if (ind1.Parms,ind1_up) ||> Array.exists2 (fun a b -> a > b) then
    failwith "slideUp"

let ind1_down = ind1.Parms |> Array.map (slideDown 1.0)
if (ind1.Parms,ind1_down) ||> Array.exists2 (fun a b -> a < b) then
    failwith "slideDown"