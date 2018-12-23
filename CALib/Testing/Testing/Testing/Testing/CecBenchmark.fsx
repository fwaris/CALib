#load "TestEnv.fsx"
open TestEnv
open CA
open CAUtils
open System.Windows.Forms

//griewank_func
let fitness (parms:Parm array) = 
    let z = parms |> Array.map(function F(v,_,_) -> v)
    let mutable s = 0.
    let mutable p = 1.
    for i in 0..z.Length-1 do
        s <- s + (z.[i] * z.[i])
        p <- p * cos(z.[i] / sqrt (float i + 1.))
    let fit = 1.0 + s / 4000.0 - p
    fit
    //for (i=0; i<nx; i++)
    //{
    //	s += z[i]*z[i];
    //	p *= cos(z[i]/sqrt(1.0+i));
    //}
    //f[0] = 1.0 + s/4000.0 - p;

let dimensions = 50
let parms = Array.create dimensions (F(0.,-600.,+600.))

let comparator  = CAUtils.Minimize
let best stp = if stp.Best.Length > 0 then stp.Best.[0].Fitness else System.Double.MaxValue
let termination step = step.Count > dimensions * 10000 || best step < 10.0e-8
let tk s = s |> Seq.truncate 100 |> Seq.toList

let runT vmx (l,f) = 
    let t = kdIpdCA vmx f comparator parms |> CARunner.run l termination 2
    l,best t,t.Count

let rs = [for i in 1 .. 51 -> runT (0.2,0.9) ("griewank",fitness) ]
