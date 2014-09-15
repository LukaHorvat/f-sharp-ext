namespace FSharpExt

module Debug =
    let logId x = 
        printfn "%A" x
        x