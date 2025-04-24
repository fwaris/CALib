///Configuration to support experimental runs with
///Cones World landscapes
//Note: the goal here to run different KD mechanisms on the exact same landscape sequences
//so that the KD performances can be compared more directly with each other
module Config.Types

type KD = WTD | IPD | SHS | STK 

type RunConfig = 
  {
    SaveFolder          : string
    EnvChngSensitivity  : int list
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
