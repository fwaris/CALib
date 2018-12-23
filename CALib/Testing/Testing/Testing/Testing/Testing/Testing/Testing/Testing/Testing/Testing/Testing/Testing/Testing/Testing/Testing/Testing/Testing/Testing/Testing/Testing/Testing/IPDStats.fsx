#r @"..\..\packages\ExcelProvider\lib\ExcelProvider.dll"
#r @"..\..\packages\MathNet.Numerics\lib\net40\MathNet.Numerics.dll"
//#r @"..\..\packages\MathNet.Numerics.FSharp\lib\net40\MathNet.Numerics.FSharp.dll"

open FSharp.ExcelProvider
open MathNet.Numerics
open MathNet.Numerics.Statistics
open MathNet.Numerics.Distributions

[<Literal>]
let file = __SOURCE_DIRECTORY__ + "\IPDWTDRun.xlsx"

type Tipd = ExcelFile<file,SheetName="IPD",HasHeaders=true>

let inline dmp (s:seq<_>) = s |> Seq.iter (printfn "%A")

let dipd = new Tipd()
let idata =  dipd.Data |> Seq.filter (fun r->r.IPGGen>0.)

let diffs = idata |> Seq.map(fun r -> r.``Pop. Size``,r.Cones,(r.IPGGen-r.WTDGen))
let gdiffs = diffs |> Seq.groupBy(fun (a,b,c)->(a,b))
let stats = gdiffs |> Seq.map (fun (d,xs) -> d, xs |> Seq.map (fun (x,y,z) -> z) |> Statistics.DescriptiveStatistics)
dmp stats
let students  = stats |> Seq.map (fun (d,st) -> d, st.Mean, st.StandardDeviation, st.Count - 1L |> float)
dmp students
let students' = students |> Seq.map (fun (d,a,b,c) -> d,a,StudentT(0.,b,c))
dmp students'
let pvals = students' |> Seq.map (fun (d,m,st) -> d, st.CumulativeDistribution(m))
dmp pvals

let exd = idata |> Seq.map (fun r-> sprintf "%0.0f-%0.0f" r.Cones r.``Pop. Size``, r.IPGGen, r.WTDGen)
dmp exd

exd |> Seq.groupBy (fun (a,b,c) -> a) |> Seq.iter (fun (s,xs) -> printfn "*****%s" s; xs |> Seq.map (fun (a,b,c) -> b,c) |> dmp)

//excel comparison
let data = 
    [
    3,6
    4,19
    5,3
    8,2
    9,14
    1,4
    2,5
    4,17
    5,1
    ]

let s1 = data |> List.map(fun (a,b) -> a-b |> float) 
let s2 = Statistics.DescriptiveStatistics(s1)
s2.Mean
s2.StandardDeviation
let t1 = StudentT.CDF(0.,s2.StandardDeviation,s2.Count  |> float, s2.Mean)
let t2 = t1 * t1