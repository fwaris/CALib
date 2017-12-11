module CA

type Tree<'a> = Leaf of 'a | Node of 'a * Tree<'a> list | Roots of Tree<'a> list

let inline parms (x:^T) = (^T:(member Parms:float []) x) //get parameters from compatible structures

type Parm = 
    | F of      v:float     * min:float     * max:float 
    | I of      v:int       * min:int       * max:int


type Id = int
type Temp = float
type Topology   = LBest | Global
type Knowledge  = Situational | Historical | Normative | Topgraphical | Domain | Other of string
type Individual<'k> = {Id:Id; Parms:float array; Fitness:float; KS:'k}
and Fitness     = float array -> float
and Comparator  = float -> float -> bool //compare two fitness values - true when 1st 'is better than' 2nd
and Population<'k>  = Individual<'k> array
and Network<'k>     = Population<'k> -> Id -> Individual<'k> array
and BeliefSpace<'k> = KnowledgeSource<'k> Tree
and Acceptance<'k>  = BeliefSpace<'k> -> Population<'k> -> Individual<'k> array
and Influence<'k>   = BeliefSpace<'k> -> Population<'k> -> Population<'k>
and Update<'k>      = BeliefSpace<'k> -> Individual<'k> array -> BeliefSpace<'k>
and KnowledgeDist<'k>   = KD of ((Population<'k>*BeliefSpace<'k>) -> Network<'k> -> (Population<'k>*BeliefSpace<'k>*KnowledgeDist<'k>))

and KnowledgeSource<'k> = 
    {
        Type        : Knowledge
        Accept      : Individual<'k> array -> Individual<'k> array * KnowledgeSource<'k>
        Influence   : Temp -> Individual<'k> -> Individual<'k>
    }

type CA<'k> =
    {
        ParmDefs                : Parm array
        Population              : Population<'k>
        Network                 : Network<'k>
        KnowlegeDistribution    : KnowledgeDist<'k>
        BeliefSpace             : BeliefSpace<'k>
        Acceptance              : Acceptance<'k>
        Influence               : Influence<'k>
        Update                  : Update<'k>
        Fitness                 : Fitness
        Comparator              : Comparator
    }

type TimeStep<'k> = {CA:CA<'k> ; Best:Individual<'k> list; Progress:float list; Count:int}
type TerminationCondition<'k> = TimeStep<'k> -> bool

            
