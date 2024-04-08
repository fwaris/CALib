///type definitions for the CA 'interface' 
//defined in a functional programming way
module rec CA

///CA structure - instance of CA that can be stepped through for optimization
type CA<'k> =
    {
        BeliefSpace             : BeliefSpace<'k>
        Acceptance              : Acceptance<'k>
        Update                  : Update<'k>
        Influence               : Influence<'k>
        Population              : Population<'k>
        Network                 : Network<'k>
        Fitness                 : Fitness
        Optimization            : OptimizationKind
        EnvChngSensitivity      : EnvChngSensitivity                                                  
    }

///how should we respond to environmental changes
//CA may or may not reset set internal state based on this setting
type EnvChngSensitivity = 

    ///CA does not adjust internal state if environment changes 
    | Insensintive 

    ///After how many environmental changes to re-adjust.
    ///A value of 1 means re-adjust to every environment change
    | Every of int                                          

///Instructs CA how to respond to environment change
type EnvChngeType = 
    | NoChange    //environment did not change
    | Adjust      //environment changed - adjust internal state accordingly
    | Track       //environment changed but only note the changes - do not adjust internal state

type OptimizationKind = Minimize | Maximize  //minimization or maximization problem

///tree structure of the belief space knowledge source
type BeliefSpace<'k> = KnowledgeSource<'k> Tree


type IncidentalBest = Marker option ref

type PrevGenParms = float[][]

///knowledge source type
type KnowledgeSource<'k> = 
    {
        ///Knowledge type identifier (Domain, Normative, etc.)
        Type        : Knowledge

        ///Acceptance function type of a knowledge source
        Accept      : EnvChngeType -> Individual<'k> array -> Individual<'k> array * KnowledgeSource<'k>

        ///Influence function type of a knowledge source
        Influence   :  IncidentalBest -> PrevGenParms -> Population<'k> -> Temperature -> Individual<'k> -> Individual<'k>
    }

type Tree<'a>        = Leaf of 'a | Node of 'a * Tree<'a> list | Roots of Tree<'a> list

type Knowledge       = Situational | Historical | Normative | Topgraphical | Domain | Other of string

///CA acceptance function type
type Acceptance<'k>  = BeliefSpace<'k> -> Population<'k> -> Individual<'k> array

///CA update function type
type Update<'k>      = EnvChngeType -> BeliefSpace<'k> -> Individual<'k> array -> BeliefSpace<'k>

///CA influence function type
type Influence<'k>   = Influence of (
                            IncidentalBest
                                -> EnvChngeType                                                //environment change signal
                                -> Population<'k> 
                                -> BeliefSpace<'k> 
                                -> Network<'k> 
                                -> Fitness 
                                -> OptimizationKind                                         
                                -> (Population<'k>*BeliefSpace<'k>*Influence<'k>))      //returns updated population, beliefSpace and influence function

///Population individual (parameterized by KS type)
type Individual<'k>  = {Id:Id; Parms:float array; Fitness:float; IsStale:bool; KS:'k}

///Population is an array of indviduals
type Population<'k>  = Individual<'k> array

///Network function type
type Network<'k>     = Population<'k> -> Id -> Individual<'k> array

///Fitness function type
type Fitness         = (float array -> float) ref

///Id of the population individual (alias to int)
type Id = int

///The level of influence to apply (alias to float)
type Temperature = float

///Parameters and fitness values extracted from 'best' individuals
type Marker = {MParms:float[]; MFitness:float}

///Structure to hold single step in a CA run
type TimeStep<'k> = {CA:CA<'k> ; Best:Marker list; Progress:float list; Count:int; EnvChngCount:int; IBest:Marker option ref}

///function type to specify the termination of a CA run
type TerminationCondition<'k> = TimeStep<'k> -> bool

///Parameter types and ranges for the fitness problem
//TODO: make this part of the CA structure
type Parm = 
    /// float parameter type
    | F of      v:float     * min:float     * max:float 

    ///integer parameter type (stepped through as whole integers by optimiztion engine)
    | I of      v:int       * min:int       * max:int
            

