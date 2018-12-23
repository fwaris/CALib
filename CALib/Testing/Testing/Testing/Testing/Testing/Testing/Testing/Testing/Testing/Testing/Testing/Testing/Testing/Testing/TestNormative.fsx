
#load "TestParms.fsx"
#load "../BeliefSpace/NormativeKS.fs"
open CA
open CAUtils

let ind0 = {Id=0; Parms=TestParms.testParms |> Array.map randomize; KS=Normative; Fitness=0.}
let ind1 = {Id=1; Parms=TestParms.testParms |> Array.map randomize; KS=Normative; Fitness=1.}

let ks = NormativeKS.create TestParms.testParms Maximize 

let acc2,ks2 = ks.Accept [|ind0|]
let acc3,ks3 = ks2.Accept [|ind1|]
if acc3.[0].Id <> 1 then failwith "Normative accept"

let ind0' = ks3.Influence ind0
if (ind0'.Parms,ind0.Parms) ||> Array.exists2 (fun a b -> a = b) then 
    failwith "Normative influence"
