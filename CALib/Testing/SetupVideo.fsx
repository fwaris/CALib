(*
Supporting script to pull in OpenCV references
for video generation
*)

#I @"..\..\packages\OpenCvSharp4.4.1.1.20191025\lib\net461"
#r @"OpenCvSharp.dll"
#r @"OpenCvSharp.Blob.dll"
#r @"OpenCvSharp.Extensions.dll" 
#r "System.Windows.Forms.DataVisualization.dll"
open System.IO
open System
open OpenCvSharp

let path = Path.Combine(__SOURCE_DIRECTORY__,@"..\..\packages\OpenCvSharp4.runtime.win.4.1.1.20191025\runtimes\win-x64\native")
System.Environment.CurrentDirectory <- path

let fullPath paths = Path.GetFullPath(Path.Combine(paths))

let dependencies = [
      @"..\..\refs"
    ]

dependencies 
|> Seq.iter (fun dep -> 
    Environment.SetEnvironmentVariable("Path",
        fullPath [|__SOURCE_DIRECTORY__;dep|] + ";" + Environment.GetEnvironmentVariable("Path"))
    )    
