module AsyncExts 
open System
open FSharp.Control
open System.Threading
open System.Threading.Tasks

module Async =
    let map f a = async.Bind(a, f >> async.Return)

module AsyncSeq =
    let mapAsyncParallelThrottled (parallelism:int) (f:'a -> Async<'b>) (s:AsyncSeq<'a>) : AsyncSeq<'b> = asyncSeq {
        use mb = MailboxProcessor.Start (ignore >> async.Return)
        use sm = new SemaphoreSlim(parallelism)
        let! err =
            s
            |> AsyncSeq.iterAsync (fun a -> async {
            let! _ = sm.WaitAsync () |> Async.AwaitTask
            let! b = Async.StartChild (async {
                try return! f a
                finally sm.Release () |> ignore })
            mb.Post (Some b) })
            |> Async.map (fun _ -> mb.Post None)
            |> Async.StartChildAsTask
        yield!
            AsyncSeq.unfoldAsync (fun (t:Task) -> async{
            if t.IsFaulted then 
                return None
            else 
                let! d = mb.Receive()
                match d with
                | Some c -> 
                    let! d' = c
                    return Some (d',t)
                | None -> return None
            })
            err
    }
(* 
      //implementation possible within AsyncSeq, with the supporting code available there       
      let mapAsyncParallelThrottled (parallelism:int) (f:'a -> Async<'b>) (s:AsyncSeq<'a>) : AsyncSeq<'b> = asyncSeq {
        use mb = MailboxProcessor.Start (ignore >> async.Return)
        use sm = new SemaphoreSlim(parallelism)
        let! err =
          s
          |> iterAsync (fun a -> async {
            do! sm.WaitAsync () |> Async.awaitTaskUnitCancellationAsError
            let! b = Async.StartChild (async {
              try return! f a
              finally sm.Release () |> ignore })
            mb.Post (Some b) })
          |> Async.map (fun _ -> mb.Post None)
          |> Async.StartChildAsTask
        yield!
          replicateUntilNoneAsync (Task.chooseTask (err |> Task.taskFault) (async.Delay mb.Receive))
          |> mapAsync id }
*)
    let mapAsyncParallelRateLimit (opsPerSecond:float) (f:'a -> Async<'b>) (s:AsyncSeq<'a>) : AsyncSeq<'b> = asyncSeq {
        let mutable tRef = DateTime.Now
        use mb = MailboxProcessor.Start (ignore >> async.Return)
        let mutable l = 0L
        let incr() = Interlocked.Increment(&l)
        let! err =
            s
            |> AsyncSeq.iterAsync (fun a -> async {
                let l' = incr() |> float
                let elapsed = (DateTime.Now - tRef).TotalSeconds 
                let rate = l' / elapsed |> min (2.0 * opsPerSecond)
                if elapsed > 60. then
                    tRef <- DateTime.Now
                    l <- 0
                    printfn $"rate {rate}"
                let diffRate = rate - opsPerSecond
                if diffRate > 0 then
                    do! Async.Sleep (int diffRate * 1000)
                let! b = Async.StartChild (async {                    
                    try return! f a
                    finally () })
                mb.Post (Some b) })
            |> Async.map (fun _ -> mb.Post None)
            |> Async.StartChildAsTask
        yield!
            AsyncSeq.unfoldAsync (fun (t:Task) -> async{
            if t.IsFaulted then 
                return None
            else 
                let! d = mb.Receive()
                match d with
                | Some c -> 
                    let! d' = c
                    return Some (d',t)
                | None -> return None
            })
            err
    }

// let collectString (rs:AsyncSeq<Nullable<int>*ChatMessage>) =
//     let mutable ls = []
//     rs
//     //|> asAsyncSeqRe
//     |> AsyncSeq.map(fun(i,x) -> x)
//     |> AsyncSeq.iter(fun x-> printfn "%A" x.Content; ls<-x.Content::ls)
//     |> Async.RunSynchronously
//     List.rev ls

