// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSharpExt

[<EntryPoint>]
let main argv = 
    let sorted = Heap.sort [100000..-1..1]
    printfn "%A" sorted
    0 // return an integer exit code
