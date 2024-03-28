namespace CAOptTest
open CA
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

module TestsCommon =

    let rastrigin maxParallelism useDE =
        let n = 5.
        let A = 10.
        let twoPi = 2. * System.Math.PI
        let An = n * A

        let rastrigin (xs:float[]) =
            let sum = xs |> Array.map(fun x -> x*x - A*cos(twoPi * x))  |> Array.sum
            An + sum

        let parms = [|for _ in 1 .. int n -> F(4.,-5.12,5.12) |]

        let mutable step = CALib.API.initCA(parms, rastrigin, Minimize, useDE=useDE)

        for i in 0 .. 15000 do 
            step <- CALib.API.Step(step,?maxParallelism=maxParallelism)

        step.Best.[0].MParms

[<TestClass>]
type CAOptTests () =

    [<TestMethod>]
    member this.Rastrigin () =
        let parms = TestsCommon.rastrigin None false
        let nearZero = parms |> Array.forall (fun x-> abs x < 1e-3)
        Assert.IsTrue(nearZero);

    [<TestMethod>]
    member this.RastriginDE () =
        let parms = TestsCommon.rastrigin None true
        let nearZero = parms |> Array.forall (fun x-> abs x < 1e-2)
        Assert.IsTrue(nearZero);

    [<TestMethod>]
    member this.RastriginDELimitParallelism () =
        let parms = TestsCommon.rastrigin (Some 10) true
        let nearZero = parms |> Array.forall (fun x-> abs x < 1e-2)
        Assert.IsTrue(nearZero);
