#load "TestParms.fsx"
#load "../BeliefSpace/DomainKS.fs"
open CA
open CAUtils

let fitness parms = parms |> Array.map parmToFloat |> Array.sum

let ind0 = {Id=0; Parms=TestParms.testParms |> Array.map randomize; KS=Domain; Fitness=0.}
let ind1 = {Id=1; Parms=TestParms.testParms |> Array.map randomize; KS=Domain; Fitness=1.}

let inds = [ind0;ind1] |> List.map (fun i-> {i with Fitness=fitness i.Parms}) |> List.sort

let ks = DomainKS.create Maximize fitness 2
;;
let acc2,ks2 = ks.Accept [|inds.[0]|]
let acc3,ks3 = ks2.Accept [|inds.[1]|]
if acc3.[0].Id <> 1 then failwith "Domain accept"

let ind0' = ks3.Influence inds.[0]
let chgs =(ind0'.Parms,inds.[0].Parms) ||> Array.map2 (fun a b -> if a = b then 0 else 1) |> Array.sum
if chgs <> 1 then
    failwith "Domain influence"

