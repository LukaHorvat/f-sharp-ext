namespace FSharpExt

module List =
    let take n list =
        let rec takeAcc n list acc =
            match list, n with
            | [], _      -> List.rev acc
            | _, 0       -> List.rev acc
            | x :: xs, _ -> takeAcc (n - 1) xs (x :: acc)
        takeAcc n list []

    let rec drop n list =
        match n, list with
        | 0, _  -> list
        | _, [] -> []
        | _, _  -> drop (n - 1) (List.tail list)