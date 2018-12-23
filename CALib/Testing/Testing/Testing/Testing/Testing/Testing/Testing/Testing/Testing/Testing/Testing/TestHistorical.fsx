#load "TestParms.fsx"
#load "../BeliefSpace/HistoricalKS.fs"
open CA
open CAUtils

let ind0 = {Id=0; Parms=TestParms.testParms |> Array.map randomize; KS=Historical; Fitness=0.}
let ind1 = {Id=1; Parms=TestParms.testParms |> Array.map randomize; KS=Historical; Fitness=1.}
let ind2 = {Id=2; Parms=TestParms.testParms |> Array.map randomize; KS=Historical; Fitness=2.}
let ind3 = {Id=3; Parms=TestParms.testParms |> Array.map randomize; KS=Historical; Fitness=3.}

let inds = [ind0; ind1; ind2; ind3]

let ks = HistoricalKS.create Maximize 3

let acc3,ks2 = (([||],ks),inds) ||> List.fold (fun (_,ks) ind -> ks.Accept [|ind|])
if acc3.[0].Id <> 3 then failwith "Historical accept"

let ind0' = ks2.Influence ind0
if (ind0'.Parms,ind0.Parms) ||> Array.exists2 (fun a b -> a = b) then 
    failwith "Historical influence"
