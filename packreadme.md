
### FSharp Script Example
```fsharp
#r "nuget: CAOpt"
open CA

let n = 5.
let A = 10.
let twoPi = 2. * System.Math.PI
let An = n * A

//objective function to be minimized
let rastrigin (xs:float[]) =
    let sum = xs |> Array.map(fun x -> x*x - A*cos(twoPi * x))  |> Array.sum
    An + sum

//define parameters (and associated ranges) that will be searched for optimality
let parms = [|for _ in 1 .. int n -> F(4.,-5.12,5.12) |] 

//Note: 'F' is for float parameters and 'I' (not shown) for integer parameters

//initialize
let mutable step = CALib.API.initCA(parms, rastrigin, Minimize)

//step through till termination criteria is met
//here it is the number of generations (steps)
//you can define your own
for i in 0 .. 15000 do 
    step <- CALib.API.Step step

step.Best.[0].MParms     //best solution parameter values
step.Best.[0].MFitness   //best solution fitness value

//rastrigin [|for _ in 1 .. int n -> 0.0|]      //true solution

//Note: A globally optimum solution is not guaranteed in evolutionary optimization methods. Usually such methods find good approximate solutions
```