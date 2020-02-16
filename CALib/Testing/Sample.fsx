#load "SetupEnv.fsx"
open CA

let n = 5.
let A = 10.
let twoPi = 2. * System.Math.PI
let An = n * A

let rastrigin (xs:float[]) =
    let sum = xs |> Array.map(fun x -> x*x - A*cos(twoPi * x))  |> Array.sum
    An + sum

let parms = [|for _ in 1 .. int n -> F(4.,-5.12,5.12) |]

let mutable step = CALib.API.initCA(parms, rastrigin, Minimize)

for i in 0 .. 5000 do 
    step <- CALib.API.Step step

step.Best.[0].MParms     //best solution parameter values
step.Best.[0].MFitness   //best solution fitness value

//rastrigin [|for _ in 1 .. int n -> 0.0|]      //true solution