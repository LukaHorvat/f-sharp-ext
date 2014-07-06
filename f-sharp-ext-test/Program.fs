// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSharpExt

[<EntryPoint>]
let main argv = 
    let rand = System.Random()
    let shuffled = List.init 10000 (fun _ -> rand.Next())
    let sorted = Heap.sort shuffled
    printfn "%A" sorted
    0 // return an integer exit code
