#load "TestEnv.fsx" //do not exec this line; it is there for intellisense only
open KDIPDGame
ld

let inline dmp s = s |> Seq.iter (printfn "%A")
let inline dmps s = s |> Seq.iter (printfn "%s")

let filterId id = function | MCoop {id1=x} | MIndv {id=x} when x=id -> true | _ -> false
let fltrAll id = function | MCoop {id1=x} | MIndv {id=x} when x=id -> true | MPout {idA=x;idB=y} when x=id || y=id -> true | _ -> false
let gen = function | MCoop {gen=g} | MIndv {gen=g} | MPout {gen=g} -> g

let flat = function 
| MCoop m -> sprintf "%d\t%d\t%A\t%A\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f" m.gen m.id1 m.ksI m.ksN m.id2 m.attr m.def m.kscom m.kslow m.dv m.stb m.coop
| MIndv i -> sprintf "%d\t%d\t%A\t%A\t%f\t%f\t%f" i.gen i.id i.ks i.ksp i.x i.y i.fit
| MPout p -> sprintf "%d\t%d\t%d\t%f" p.gen p.idA p.idB p.payoff

let getId i = 
    ld 
    |> Seq.filter (filterId i)
    |> Seq.sortBy gen

let traceId i =
    getId i
    |> Seq.map flat
    |> Seq.iter (printfn "%s")

traceId 10
traceId 11


ld |> Seq.filter (fltrAll 10) |> Seq.filter (gen>>((=)15)) |> Seq.map flat |> dmps
ld |> Seq.filter (fltrAll 8) |> Seq.filter (gen>>((=)15)) |> Seq.map flat |> dmps
