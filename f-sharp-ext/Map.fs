namespace FSharpExt

module Map =
    let findOrDefault k d m =
        let opt = Map.tryFind k m
        if Option.isNone opt then d else Option.get opt