# CALib
Cultural Algorithms optimization framework

## Set up
- Install Visual Studio (VS) 2019 (or later)
- You can get the free community edition from here:  https://visualstudio.microsoft.com/vs/community/
- During setup, select support for **F# language** and **.Net Core**.
The Visual Studio setup utility has 'Workloads' that bundle commonly used features. Select appropriate boxes as shown below to get F# and .Net core installed:

- [More detailed PDF is here](./CALib%20Installation%20Guide.pdf)


![visual studio setup utility view](imgs/setup.jpg)

- After VS is installed, open the 'solution' file CALib.sln with Visual Studio

- In the 'Solution Explorer' window right click on the root of the tree and select "Restore NuGet Packages" as shown below:


![restore nuget packages for the solution](imgs/restore.jpg)

- After the package load is complete, compile solution to check configuration validity

## Simple sample
A sample for Rastrigin function is given [Sample.fsx](CALib/testing/Samples.fsx).

'''fsharp
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
'''

## Cones World Visualization
Open "ConesWorldVisualization.fsx" script in editor and select all text and hit Alt-Enter to run script

The script should open with animated windows that look like the image below.

![Stag-Hunt](imgs/sh_screen_shot.jpg)

## F# Language Resources
Start with https://fsharp.org/ to access the learning resources available for the F# language

## Projects
The solution has two projects:

* CALib - this can be used in two ways:

   1. Compiled to a DLL and linked with a .Net application to solve optimization problems

   2. Interactively with F# interactive (.fsx) script files, for research and experimentation purposes

- CaLibCore - a version of CALib that runs on Linux (with .Net Core). Its main purpose is to conduct experiments on the Wayne State grid computing environment
