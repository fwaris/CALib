module Runs.Types
open DF1
open CA
open System.IO

//a single program run executes according to this config

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
    Ws                  : WorldState
    A                   : float
    EnvChngSensitivity  : int
    Net                 : NetId
    Landscape           : int
    SampleNum           : int
    EnvCh               : bool
    Steps               : Step array
  }

type GenStats = 
    {
        KD              : string
        EnvCgnSnstvy    : int
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


