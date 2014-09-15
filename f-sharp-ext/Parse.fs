namespace FSharpExt

module Parse = 
    type Parser = char list -> (char list * char list) option
    
    //Parser combinators
    let oneOrNone parser str = 
        match parser str with
        | None -> Some([], str)
        | res -> res
    
    let oneOrMore parser str = 
        let one = parser str
        match one with
        | None -> None
        | x -> 
            let rec moreAcc (Some(leftAcc, rest) as acc) = 
                let more = parser rest
                match more with
                | None -> acc
                | Some(left, right) -> moreAcc <| Some(leftAcc @ left, right)
            moreAcc x
    
    let noneOrMore parser str = 
        match oneOrMore parser str with
        | None -> Some([], str)
        | x -> x
    
    let (|||) parser1 parser2 str = 
        match parser1 str with
        | None -> parser2 str
        | x -> x
    
    let (|>|) parser1 parser2 str = 
        match parser1 str with
        | None -> None
        | Some(left, rest) -> 
            match parser2 rest with
            | None -> None
            | Some(right, rest) -> Some(left @ right, rest)
    
    let (|++|) parser1 parser2 str = 
        match parser1 str with
        | None -> None
        | Some(list, rest) -> 
            match parser2 rest with
            | None -> Some(list, rest)
            | Some(res, rest) -> Some(list @ [ res ], rest)
    
    let collect parser str = 
        match parser str with
        | None -> None
        | Some(res, rest) -> Some([ res ], rest)
    
    let peek parser str = 
        match parser str with
        | None -> None
        | _ -> Some([], str)
    
    let ifBefore parser1 parser2 = parser1 |>| peek parser2
    
    let charCondition f str = 
        match str with
        | x :: xs when f x -> Some([ x ], xs)
        | _ -> None
    
    let char c = charCondition ((=) c)
    
    let string word = 
        List.ofSeq word
        |> List.map char
        |> List.reduce (|>|)
    
    let letter = charCondition (fun c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')
    let digit = charCondition (fun c -> c >= '0' && c <= '9')
    let anyOf chars = charCondition (Func.flip Seq.contains chars)
    let whitespace = anyOf [ ' '; '\t' ]
    let word = oneOrMore letter
    let number = oneOrMore digit
    let noneOf chars = charCondition (Func.flip Seq.contains chars >> not)
    
    let any chars = 
        match chars with
        | x :: xs -> Some([ x ], xs)
        | _ -> None
    
    let not parser str = 
        match parser str with
        | None -> any str
        | _ -> None
    
    let nothing chars = 
        match chars with
        | [] -> Some([], [])
        | _ -> None
