
open System.IO

//tail function to read log files; cancel token to stop
let tail filter (cts:System.Threading.CancellationTokenSource) file fPost =
    let rdr = new StreamReader(new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
    let off = ref rdr.BaseStream.Length
    let go = ref true
    let loop = 
        async {
            try
                while true do
                    do! Async.Sleep 100
                    if rdr.BaseStream.Length <> !off then
                        rdr.BaseStream.Seek(!off,SeekOrigin.Begin) |> ignore
                        let mutable l = rdr.ReadLine()
                        while l <> null do
                            if not cts.IsCancellationRequested then
                                if filter l then fPost l
                                l <- rdr.ReadLine()
                        off := rdr.BaseStream.Position
            finally
                    rdr.Close()
                    printfn "closed tail %s" file
            }
    Async.Start(loop,cts.Token)

(*usage: 
let file = @"D:\calib\dsst_stats\Stats.txt"
let cts = new System.Threading.CancellationTokenSource()

do tail (fun _ -> true) cts file (printfn "%A")

// cts.Cancel()     //cancel token stop tial
*)