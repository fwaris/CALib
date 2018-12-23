module CA

open System.Runtime.CompilerServices

type Tree<'a> = Leaf of 'a | Node of 'a * Tree<'a> list | Roots of Tree<'a> list

let inline parms (x:^T) = (^T:(member Parms:float []) x) //get parameters from compatible structures

type Parm = 
    | F of      v:float     * min:float     * max:float 
    | I of      v:int       * min:int       * max:int

type Id = int
type Temp = float
type Knowledge  = Situational | Historical | Normative | Topgraphical | Domain | Other of string
type Individual<'k> = {Id:Id; Parms:float array; Fitness:float; KS:'k}
and Fitness     = (float array -> float) ref
and Comparator  = float -> float -> bool //compare two fitness values - true when 1st 'is better than' 2nd
and Population<'k>  = Individual<'k> array
and Network<'k>     = Population<'k> -> Id -> Individual<'k> array
and BeliefSpace<'k> = KnowledgeSource<'k> Tree
and Acceptance<'k>  = BeliefSpace<'k> -> Population<'k> -> Individual<'k> array
//and Influence<'k>   = BeliefSpace<'k> -> Population<'k> -> Population<'k>
and Update<'k>      = bool -> BeliefSpace<'k> -> Individual<'k> array -> BeliefSpace<'k>
and Influence<'k>   = Influence of (bool -> Population<'k> -> BeliefSpace<'k> -> Network<'k> -> Fitness -> Comparator -> (Population<'k>*BeliefSpace<'k>*Influence<'k>))

and KnowledgeSource<'k> = 
    {
        Type        : Knowledge
        Accept      : bool -> Individual<'k> array -> Individual<'k> array * KnowledgeSource<'k>
        Influence   : Temp -> Individual<'k> -> Individual<'k>
    }


type CA<'k> =
    {
        BeliefSpace             : BeliefSpace<'k>
        Acceptance              : Acceptance<'k>
        Update                  : Update<'k>
        Influence               : Influence<'k>
        Population              : Population<'k>
        Network                 : Network<'k>
        Fitness                 : Fitness
        Comparator              : Comparator
    }

type Marker = {MParms:float[]; MFitness:float}
type TimeStep<'k> = {CA:CA<'k> ; Best:Marker list; Progress:float list; Count:int}
type TerminationCondition<'k> = TimeStep<'k> -> bool

            
