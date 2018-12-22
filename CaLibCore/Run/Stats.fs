module Runs.Stat
open CA
open FSharp.Collections.ParallelSeq
open Runs.Environment
open Runs.Types
open System.IO

let segAt ca fSeg indv = 
    Social.segregationAt                        //Schelling-like segregation measure
        SEG_RAD                                 //radius of neighborhood
        (1.0 / float Social.ksSegments.Length)  //proportion of each segment or group at start
        Social.ksSegments                       //list of segments
        ca                                      //current state of CA
        fSeg
        indv

let dfsnAt ca p = Social.diffusionAt ca p

let segF primKS indv  = primKS indv.KS |> Social.ksNum

let popKS st primKS =  st.CA.Population |> Array.map(fun i-> primKS i.KS |> Social.ksNum)

let socMetrics st f = 
    st.CA.Population |> PSeq.map (fun i -> i.Id,segAt st.CA (segF f) i) |> PSeq.toArray |> Array.sortBy fst |> Array.map snd,
    st.CA.Population |> PSeq.map (fun i -> i.Id,dfsnAt st.CA i) |> PSeq.toArray |> Array.sortBy fst |> Array.map snd

let kdId = function 
    | WtdSt _ -> "WTD"
    | IpdSt _ -> "IPD"
    | ShSt  _ -> "SH"
    | StkSt _ -> "STK"

let genCount lndscpCfg = 
    match Array.head lndscpCfg.Steps with 
    | WtdSt (st,_) -> st.Count
    | IpdSt (st,_) -> st.Count
    | ShSt  (st,_) -> st.Count
    | StkSt (st,_) -> st.Count

let stepSocialMetrics = function
    | WtdSt (st,f) -> socMetrics st f
    | IpdSt (st,f) -> socMetrics st f
    | ShSt  (st,f) -> socMetrics st f
    | StkSt (st,f) -> socMetrics st f

let stepKSAssigns = function
    | WtdSt (st,f) -> popKS st f
    | IpdSt (st,f) -> popKS st f
    | ShSt  (st,f) -> popKS st f
    | StkSt (st,f) -> popKS st f

let stepBestFitness = function
    | WtdSt (st,_) -> st.Best.[0].MFitness
    | IpdSt (st,_) -> st.Best.[0].MFitness
    | ShSt  (st,_) -> st.Best.[0].MFitness
    | StkSt (st,_) -> st.Best.[0].MFitness

let statRec rsc lndscpCfg step =
    let iSeg,iDffsn = 
        if rsc.CalcSocMetrics then
            stepSocialMetrics step
        else
            [|-1.0|],
            [|-1.0|]

    let seg,dffsn = 
        if rsc.CalcSocMetrics then
            iSeg   |> Array.average,
            iDffsn |> Array.average
        else
            -1.0,-1.0
    {
        Sample=lndscpCfg.SampleNum
        KD=kdId step
        LandscapeNum=lndscpCfg.Landscape
        A=lndscpCfg.A
        GenCount=genCount lndscpCfg
        Seg=seg
        Dffsn=dffsn
        Max=stepBestFitness step
        Net=sprintf "%A" lndscpCfg.Net
        IndvSeg = iSeg
        IndvDfsn = iDffsn
        IndvKs = stepKSAssigns step
    }

let initStatFile rsc fileName = 
    if Directory.Exists rsc.SaveFolder |> not then Directory.CreateDirectory rsc.SaveFolder |> ignore
    let path = Path.Combine(rsc.SaveFolder,fileName)
    if File.Exists path |> not then 
        use fn = new StreamWriter(File.OpenWrite(path))
        fn.WriteLine("Sample\tKD\tLandscapeNum\tA\tGenCount\tMax\tSeg\tDffsn\tNet\tIndvSeg\tIndvDffsn\tIndvKS")

let writeStats rsc fileName ls =
    let path = Path.Combine(rsc.SaveFolder,fileName)
    use fn = File.AppendText(path)
    let line = 
        sprintf "%d\t%s\t%d\t%f\t%d\t%f\t%f\t%f\t%s" 
            ls.Sample ls.KD ls.LandscapeNum ls.A ls.GenCount ls.Max ls.Seg ls.Dffsn ls.Net
    fn.Write(line)
    fn.Write("\t")
    ls.IndvSeg |> Array.iter (fun x -> fn.Write(x); fn.Write("|"))
    fn.Write("\t")
    ls.IndvDfsn |> Array.iter (fun x -> fn.Write(x); fn.Write("|"))
    fn.Write("\t")
    ls.IndvKs |> Array.iter (fun x -> fn.Write(x); fn.Write("|"))
    fn.WriteLine()
