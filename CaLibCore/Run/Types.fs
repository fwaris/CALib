module Runs.Types

open DF1
open CA
open System.IO
open KDIPDGame

type KD = WTD | IPD | SH | STK

//a single program run executes according to this config
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
type RunId = {Id:string; SampleNum:int; Net:NetId; A:float}
type WorldState = {Id:string; W:World; M:Cone; F:float[]->float; EnvChangeCount:int}

type GenStats = 
    {
        KD              : string
        A               : float
        Sample          : int
        LandscapeNum    : int
        GenCount        : int
        Net             : string
        Max             : float
        Seg             : float
        Dffsn           : float
        IndvSeg         : float[]
        IndvDfsn        : float[]
        IndvKs          : int[]
    }

type RunState<'k> =
  {
    KD        : string
    A         : float
    PrimKS    : 'k->Knowledge
    Ws        : WorldState
    EnvCh     : bool
    Step      : TimeStep<'k>
    Landscape : int
    SampleNum : int
    StrWComm  : StreamWriter
    //StrWRun   : StreamWriter
  }


