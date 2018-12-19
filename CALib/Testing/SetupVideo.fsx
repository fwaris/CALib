#I @"..\..\packages\OpenCvSharp3-AnyCPU\lib\net461"
#r @"OpenCvSharp.dll"
#r @"OpenCvSharp.Blob.dll"
#r @"OpenCvSharp.Extensions.dll" 
#r "System.Windows.Forms.DataVisualization.dll"
open System.IO
open System
open OpenCvSharp

let path = Path.Combine(__SOURCE_DIRECTORY__,@"..\..\packages\OpenCvSharp3-AnyCPU\runtimes\win10-x64\native")
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
