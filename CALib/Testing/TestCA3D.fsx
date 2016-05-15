#r @"../../packages/FSharp.Collections.ParallelSeq.1.0.2\lib\net40\FSharp.Collections.ParallelSeq.dll"
#load "../CA.fs"
#load "../CAUtils.fs"
#load "../BeliefSpace/SituationalKS.fs"
#load "../BeliefSpace/NormativeKS.fs"
#load "../BeliefSpace/HistoricalKS.fs"
#load "../BeliefSpace/DomainKS.fs"
#load "../KnowledgeDistribution/KDBase.fs"
#load "../KnowledgeDistribution/KDGame.fs"
#load "../CARunner.fs"
open CA

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
let beliefSpace = CARunner.defaultBeliefSpace parms comparator fitness
//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let pop         = CAUtils.createPop parms 1000 beliefSpace true

let ca =
    {
        Population           = pop
        Network              = CAUtils.l4BestNetwork
        KnowlegeDistribution = KDGame.gtKnowledgeDist
        // KD(KDBase.knowledgeDistribution KDBase.majority)
                              
        BeliefSpace          = beliefSpace
        Acceptance           = CARunner.acceptance 5 comparator
        Influence            = CARunner.influence
        Update               = CARunner.update
        Fitness              = fitness
        Comparator           = comparator
    }

let termination step = step.Count > 1000

(*
let r = (CARunner.run  termination 2 ca)
r.Best
r.CA.Population |> Seq.countBy (fun p -> p.KS)
r.CA.Fitness r.Best.[0].Parms
*)

