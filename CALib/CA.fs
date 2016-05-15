module CA

type Tree<'a> = Leaf of 'a | Node of 'a * Tree<'a> list | Roots of Tree<'a> list

type Parm = 
    | F of      v:float     * min:float     * max:float 
    | F32 of    v:float32   * min:float32   * max:float32
    | I of      v:int       * min:int       * max:int
    | I64 of    v:int64     * min:int64     * max:int64

type Id = int
type Topology   = LBest | Global
type Knowledge  = Situational | Historical | Normative | Topgraphical | Domain | Other of string
type Individual = {Id:Id; Parms:Parm array; Fitness:float; KS:Knowledge}
and Fitness     = Parm array -> float
and Comparator  = float -> float -> bool //compare two fitness values - true when 1st 'is better than' 2nd
and Population  = Individual array
and Network     = Population -> Id -> Individual array
and BeliefSpace = KnowledgeSource Tree
and Acceptance  = BeliefSpace -> Population -> Individual array
and Influence   = BeliefSpace -> Population -> Population
and Update      = BeliefSpace -> Individual array -> BeliefSpace
and KnowledgeDist   = KD of ((Population*BeliefSpace) -> Network -> (Population*BeliefSpace*KnowledgeDist))

and KnowledgeSource = 
    {
        Type        : Knowledge
        Accept      : Individual array -> Individual array * KnowledgeSource
        Influence   : Individual -> Individual
    }

type CA =
    {
        Population              : Population
        Network                 : Network
        KnowlegeDistribution    : KnowledgeDist
        BeliefSpace             : BeliefSpace
        Acceptance              : Acceptance
        Influence               : Influence
        Update                  : Update
        Fitness                 : Fitness
        Comparator              : Comparator
    }

type TimeStep = {CA:CA ; Best:Individual list; Progress:float list; Count:int}
type TerminationCondition = TimeStep -> bool

            
