#I @"..\..\packages\OpenCvSharp3-AnyCPU\lib\net46"
#r @"OpenCvSharp.dll"
#r @"OpenCvSharp.Blob.dll"
#r @"OpenCvSharp.Extensions.dll" 
#r "System.Windows.Forms.DataVisualization.dll"
open System.IO
open OpenCvSharp

let path = Path.Combine(__SOURCE_DIRECTORY__,@"..\..\packages\OpenCvSharp3-AnyCPU\NativeDlls\x64")
System.Environment.CurrentDirectory <- path

