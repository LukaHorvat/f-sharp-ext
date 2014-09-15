namespace FSharpExt

module List = 
    let take n list = 
        let rec takeAcc n list acc = 
            match list, n with
            | [], _ | _, 0 -> List.rev acc
            | x :: xs, _ -> takeAcc (n - 1) xs (x :: acc)
        takeAcc n list []
    
    let rec drop n list = 
        match n, list with
        | 0, _ -> list
        | _, [] -> []
        | _, _ -> drop (n - 1) (List.tail list)
    
    let splitAt n list = (take n list, drop n list)
    let singleton x = [ x ]
    let concatMap fn list = List.map fn list |> List.concat
    let contains x list = List.exists ((=) x) list
    
    let rec foldUntil concatNext acc start = 
        let (newAcc, maybeNext) = concatNext acc start
        match maybeNext with
        | Some next -> foldUntil concatNext newAcc next
        | None -> acc
    
    /// <summary>
    /// splitFold takes three parameters. A function f, the starting accumulator and a list to fold.
    /// The function f takes two paramters: the current accumulator and the rest of the list.
    /// It produces the updated accumulator and the remainder of the list that it didn't process.
    /// splitFold repeatedly applies the function f to the remainders of the list until the function
    /// returns an empty list as a remainder.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="acc"></param>
    /// <param name="list"></param>
    let rec splitFold (f : 'a -> 'b list -> ('a * 'b list)) (acc : 'a) (list : 'b list) : 'a = 
        match list with
        | [] -> acc
        | _ -> Func.uncurry (splitFold f) <| f acc list
    
    let splitOn f list = 
        let rec splitOnRec f list = 
            match list with
            | [] -> [ [] ]
            | x :: xs when f x -> ([] :: splitOnRec f xs)
            | x :: xs -> 
                let (first :: rest) = splitOnRec f xs
                (x :: first) :: rest
        
        let rec removeLastEmpty list = 
            match list with
            | [] :: [] | [] -> []
            | x :: xs -> x :: removeLastEmpty xs
        
        match splitOnRec f list |> removeLastEmpty with
        | [] :: xs -> xs
        | x -> x
    
    let rec splitAfter f list = 
        match list with
        | [] -> [ [] ]
        | x :: xs when f x -> 
            if xs = [] then ([ [ x ] ])
            else ([ x ] :: splitAfter f xs)
        | x :: xs -> 
            let (first :: rest) = splitAfter f xs
            (x :: first) :: rest
    
    let splitBefore f list = 
        let rec splitBeforeRec f list = 
            match list with
            | [] -> [ [] ]
            | x :: xs when f x -> 
                let (first :: rest) = splitBeforeRec f xs
                [] :: (x :: first) :: rest
            | x :: xs -> 
                let (first :: rest) = splitBeforeRec f xs
                (x :: first) :: rest
        match splitBeforeRec f list with
        | [] :: xs -> xs
        | x -> x
    
    let rec mapOption f list = 
        match list with
        | x :: xs -> 
            let m = f x
            match m with
            | None -> mapOption f xs
            | Some y -> y :: mapOption f xs
        | [] -> []

    let rec intersperse x list =
        match list with
        | [] -> []
        | [y] -> [y]
        | y :: ys -> y :: x :: (intersperse x ys)

    let dropBack n list =
        let minusN = drop n list
        let rec takeAcc (x :: xs) counter =
            match counter with
            | [_]-> [x]
            | _ -> x :: takeAcc xs (List.tail counter)
        takeAcc list minusN