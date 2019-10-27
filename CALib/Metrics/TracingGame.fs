/// diagnostic code for game specific tracing 
/// (for diagnostic use only)
module TracingGame
open Tracing
open CA

type Link = Set<Id> * float

type SG<'k> = 
  {
    Pop:Population<'k>
    Net:Network<'k>
    Vmin:float
    Links : (Link list)[]
  }

//network weights
//let obsNetW,fpNetW = Observable.createObservableAgent<SG<'k>> cts.Token

//game cooperation 
type LIndv = {id:int; x:float; y:float; ks:Knowledge; ksp:Knowledge; gen:int; fit:float}
type LCoop = {id1:int; id2:int; attr:float; def:float; 
              kscom:float; kslow:float; dv:float; gen:int; coop:float
              stb:float;
              ksI:Knowledge; ksN:Knowledge}
type LPout = {gen:int; idA:int; idB:int; payoff:float }

type Log = MIndv of LIndv | MCoop of LCoop | MPout of LPout 

let obsGame,fpGame = Observable.createObservableAgent<Log> cts.Token