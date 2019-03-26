module Config.Types

type KD = WTD | IPD | SH | SHS | STK

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
