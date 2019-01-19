// Learn more about F# at http://fsharp.org

open System
open System.IO.Compression


[<EntryPoint>]
let main argv =
    printfn "Compressing folder"
    ZipFile.CreateFromDirectory(@"/wsu/home/ar/ar86/ar8623/calib/jobout_amp", "/wsu/home/ar/ar86/ar8623/calib/jobsout.zip")  
    0 // return an integer exit code
    