module Runs.Types
open DF1
open CA
open System.IO

type KD = WTD | IPD | SH | SHS | STK

let envChngSnstvy = function 0 -> Insensintive | x -> Every x

//a single program run executes according to this config
type RunConfig = 
  {
    SaveFolder          : string
    EnvChngSensitivity  : int
    Restartable         : bool
    KDs                 : KD list
    PopulationSize      : int
    NumCones            : int
    RunToMax            : bool
    CalcSocMetrics      : bool
    MaxGen              : int
    NumLandscapes       : int
    Samples             : int
    DistTh              : float
    AValues             : float list
    ChangeHeight        : bool
    ChangeRadius        : bool
    ChangeLoc           : bool
  }

type NetId = Square | Hexagon | Octagon
//type RunId = {Id:string; SampleNum:int; Net:NetId; A:float}
type WorldState = {W:World; M:Cone; F:float[]->float; EnvChangeCount:int}

type Step = 
    | WtdSt of TimeStep<Knowledge> * (Knowledge->Knowledge)
    | IpdSt of TimeStep<KDIPDGame.IpdKS> * (KDIPDGame.IpdKS->Knowledge)
    | ShSt  of TimeStep<KDStagHunt.ShKnowledge> *  (KDStagHunt.ShKnowledge->Knowledge)
    | ShSSt  of TimeStep<KDStagHuntStatic.ShKnowledge> *  (KDStagHuntStatic.ShKnowledge->Knowledge)
    | StkSt of TimeStep<KDStackelberg.StkKnowledge> * (KDStackelberg.StkKnowledge->Knowledge)

type LandscapeConfig =
  {
    Ws        : WorldState
    A         : float
    Net       : NetId
    Landscape : int
    SampleNum : int
    EnvCh     : bool
    Steps     : Step array
  }

type GenStats = 
    {
        KD              : string
        A               : float
        Sample          : int
        LandscapeNum    : int
        GenCount        : int
        Net             : string
        Best            : float
        Max             : float
        Seg             : float
        Dffsn           : float
        IndvSeg         : float[]
        IndvDfsn        : float[]
        IndvKs          : int[]
    }


