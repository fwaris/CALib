(*
Set up Cultural Algorithms (CA) for interactive use
This script references all the required DLLs and loads the code 
so that CA can be use interactively with F# scripting

Normally this is called from another script and not used directly

It only needs to be modified 
if packages are updated or new code is added
*)

#r @"..\..\packages\FSharp.Collections.ParallelSeq.1.1.2\lib\net45\FSharp.Collections.ParallelSeq.dll"
#r @"..\..\packages\FSharp.Charting.2.1.0\lib\net45\FSharp.Charting.dll"
#r @"..\..\packages\FSharp.Control.AsyncSeq.2.0.22\lib\net45\FSharp.Control.AsyncSeq.dll"
#r "System.Windows.Forms.DataVisualization"
#r "System.IO.Compression"
#r "System.IO.Compression.FileSystem"
#r "System.IO.Compression.ZipFile"
#r "System.Windows"
#r "System.Drawing"
#load "../CA.fs"
#load "../Probability.fs"
#load "../CAUtils.fs"
#load "../CAEvolve.fs"
#load "../Metrics/ObservableExt.fs"
#load "../Metrics/Metrics.fs"
#load "../Metrics/Tracing.fs"
#load "../Metrics/TracingGame.fs"
#load "../Metrics/Social.fs"
#load "../BeliefSpace/SituationalKS.fs"
#load "../BeliefSpace/NormativeKS.fs"
#load "../BeliefSpace/HistoricalKS.fs"
#load "../BeliefSpace/DomainKS.fs"
#load "../BeliefSpace/DiffEvolutionKS.fs"
#load "../BeliefSpace/KMeans.fs"
#load "../BeliefSpace/TopographicKS.fs"
//#load "../KnowledgeDistribution/KDSimpleMajority.fs"
//#load "../KnowledgeDistribution/KDGame.fs"
//#load "../KnowledgeDistribution/KDLocallyWeightedMajority.fs"
//#load "../KnowledgeDistribution/KDHedonicGame.fs"
#load "../KnowledgeDistribution/MetaLrn.fs"
#load "../KnowledgeDistribution/MetaLrn2.fs"
#load "../KnowledgeDistribution/KDWeightedMajority.fs"
#load "../KnowledgeDistribution/KDContinousStrategyGame.fs"
#load "../KnowledgeDistribution/KDIpDGame.fs"
#load "../KnowledgeDistribution/KDIPD.fs"
#load "../KnowledgeDistribution/Schelling.fs"
#load "../KnowledgeDistribution/KDStagHunt.fs"
#load "../KnowledgeDistribution/KDStagHuntStatic.fs"
#load "../KnowledgeDistribution/KDStackelberg.fs"
#load "../CARunner.fs"
#load "../DF1.fs"
#load @"../../CaLibCore/Run/ConfigTypes.fs"
#load @"../../CaLibCore/Run/Types.fs"
#load @"../../CaLibCore/Run/Environment.fs"
#load @"../../CaLibCore/Run/Stats.fs"
#load @"../Utilities/FPGrowth.fs"
#load @"../../CaLibCore/Utilities/Community.fs"
//#load @"../../CaLibCore/Run/RunDynamicSeq.fs"


