namespace FSharpExt

module StateParse = 
    type StateParser<'a> = 'a -> char list -> (char list * char list * 'a) option
    
    let oneOrNone parser state str = 
        match parser state str with
        | None -> Some([], str, state)
        | res -> res
    
    let oneOrMore parser state str = 
        let one = parser state str
        match one with
        | None -> None
        | x -> 
            let rec moreAcc (Some(leftAcc, rest, currentState) as acc) = 
                let more = parser currentState rest
                match more with
                | None -> acc
                | Some(left, right, newState) -> moreAcc <| Some(leftAcc @ left, right, newState)
            moreAcc x
    
    let noneOrMore parser state str = 
        match oneOrMore parser state str with
        | None -> Some([], str, state)
        | x -> x
    
    let (|.||) parser1 parser2 state str = 
        match parser1 state str with
        | None -> parser2 state str
        | x -> x
    
    let (|.>|) parser1 parser2 state str = 
        match parser1 state str with
        | None -> None
        | Some(left, leftRest, leftState) -> 
            match parser2 leftState leftRest with
            | None -> None
            | Some(right, rest, newState) -> Some(left @ right, rest, newState)
    
    let peek parser state str = 
        match parser state str with
        | None -> None
        | _ -> Some([], str, state)
    
    let ifBefore parser1 parser2 = parser1 |.>| peek parser2
    
    let any state chars = 
        match chars with
        | x :: xs -> Some([ x ], xs, state)
        | _ -> None
    
    let not parser state str = 
        match parser state str with
        | None -> any state str
        | _ -> None
    
    let withStateFunc f parser state str = 
        match parser str with
        | None -> None
        | Some(left, right) -> Some(left, right, f state)

    let toStateless initial parser str =
        match parser initial str with
        | None -> None
        | Some(left, rest, _) -> Some(left, rest)
