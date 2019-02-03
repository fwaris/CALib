// Learn more about F# at http://fsharp.org

open System
open System.IO.Compression


[<EntryPoint>]
let main argv =
    let folder,file =
        match argv with 
        |[|folder;file|] -> folder, file
        |_               -> failwithf "usage: cmprs <folder to compress> <outputFilePath>"
    printfn "Compressing folder"
    //ZipFile.CreateFromDirectory(@"/wsu/home/ar/ar86/ar8623/calib/jobout_amp", "/wsu/home/ar/ar86/ar8623/calib/jobsout.zip")  
    ZipFile.CreateFromDirectory(folder, file)  
    0 // return an integer exit code
    