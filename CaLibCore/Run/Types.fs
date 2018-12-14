module Runs.Types

open DF1
open CA
open System.IO

type KD = WTD | IPD | SH | STK

type RunsConfig= 
  {
    SaveFolder    : string
    KDs           : KD list
    PopulationSize : int
    NumCones       : int
    RunToMax      : bool
    CalcSocMetrics : bool
    MaxGen        : int
    NumLandscapes : int
    Samples       : int
    DistTh        : float
    AValues       : float list
    ChangeHeight  : bool
    ChangeRadius  : bool
    ChangeLoc     : bool
  }

type NetId = Square | Hexagon | Octagon
type RunId = {Id:string; Run:int; Net:NetId; A:float}
type WorldState = {Id:string; W:World; M:Cone; F:float[]->float; EnvChangeCount:int}

type LandscapeStats = 
    {
        ConfigRun       : int
        Id              : string
        LandscapeNum    : int
        A               : float
        GenCount        : int
        Max             : float
        Seg             : float
        Dffsn           : float
        Net             : string
        IndvSeg         : float[]
        IndvDfsn        : float[]
        IndvKs          : int[]
    }

type RunState<'k> =
  {
    Id        : string
    A         : float
    PrimKS    : 'k->Knowledge
    Ws        : WorldState
    EnvCh     : bool
    Step      : TimeStep<'k>
    Landscape : int
    SampleNum : int
    StrWComm  : StreamWriter
    StrWRun   : StreamWriter
  }


