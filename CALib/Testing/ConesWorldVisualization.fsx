(*
Cones World, dynamic landscape visualization with
of many different knowledge distribution mechanisms
operating simultaneously

Note: To run this script select all the text in the script and hit Alt-Enter

*)

#load "TraceDynamic.fsx"
open TraceDynamic

createForms rsc
async {
    let! r = Async.Catch runner
    match r with
    | Choice1Of2 _ -> ()
    | Choice2Of2 err -> printfn "%A" err
} |> Async.Start

