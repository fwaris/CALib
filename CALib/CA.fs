module rec CA

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

type BeliefSpace<'k> = KnowledgeSource<'k> Tree

type KnowledgeSource<'k> = 
    {
        Type        : Knowledge
        Accept      : bool -> Individual<'k> array -> Individual<'k> array * KnowledgeSource<'k>
        Influence   : Temp -> Individual<'k> -> Individual<'k>
    }

type Tree<'a>        = Leaf of 'a | Node of 'a * Tree<'a> list | Roots of Tree<'a> list
type Knowledge       = Situational | Historical | Normative | Topgraphical | Domain | Other of string
type Acceptance<'k>  = BeliefSpace<'k> -> Population<'k> -> Individual<'k> array
type Update<'k>      = bool -> BeliefSpace<'k> -> Individual<'k> array -> BeliefSpace<'k>

type Influence<'k>   = Influence of (
                            bool                                                        //environment change signal
                                -> Population<'k> 
                                -> BeliefSpace<'k> 
                                -> Network<'k> 
                                -> Fitness 
                                -> Comparator 
                                -> (Population<'k>*BeliefSpace<'k>*Influence<'k>))      //returns updated population, beliefSpace and influence function

type Population<'k>  = Individual<'k> array
type Individual<'k>  = {Id:Id; Parms:float array; Fitness:float; KS:'k}
type Network<'k>     = Population<'k> -> Id -> Individual<'k> array
type Fitness         = (float array -> float) ref
type Comparator      = float -> float -> bool //compare two fitness values - true when 1st 'is better than' 2nd

type Id = int
type Temp = float
type Marker = {MParms:float[]; MFitness:float}
type TimeStep<'k> = {CA:CA<'k> ; Best:Marker list; Progress:float list; Count:int}
type TerminationCondition<'k> = TimeStep<'k> -> bool

type Parm = 
    | F of      v:float     * min:float     * max:float 
    | I of      v:int       * min:int       * max:int
            
let inline parms (x:^T) = (^T:(member Parms:float []) x) //get parameters from compatible structures
