open FSharpExt

[<EntryPoint>]
let main argv =
    let x : Matrix<_, Three, Two> = 
        Matrix.readAligned [ [ 1.0; 2.0 ]
                             [ 2.0; 3.0 ]
                             [ 3.0; 4.0 ] ]
    
    let y : Matrix<_, Two, Three> = 
        Matrix.readAligned [ [ 1.0; 2.0; 3.0 ]
                             [ 5.0; 6.0; 7.0 ] ]
    printfn "%s" (Matrix.show (Matrix.mult x y))
    System.Console.ReadLine() |> ignore
    0
