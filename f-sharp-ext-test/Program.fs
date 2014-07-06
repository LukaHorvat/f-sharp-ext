// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSharpExt

[<EntryPoint>]
let main argv = 
    let rand = System.Random()
    let shuffled = List.init 100000 (fun _ -> rand.Next())
    let sorted = PriorityQueue.ofList shuffled
    printfn "%d" (PriorityQueue.peek sorted)
    0 // return an integer exit code
