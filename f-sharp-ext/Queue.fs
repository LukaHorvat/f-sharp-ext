namespace FSharpExt

type Queue<'a> = Queue of 'a list * 'a list

module Queue =
    let empty = Queue([], [])

    let balance (Queue(exit, enter)) =
        match exit with
        | [] -> Queue(List.rev enter, [])
        | _  -> Queue(exit, enter)

    let peek (Queue(exit, enter)) =
        match exit with
        | []      -> failwith "Queue is empty"
        | x :: xs -> x

    let rec remove (Queue(exit, enter)) =
        match exit with
        | []      -> failwith "Queue is empty"
        | x :: xs -> Queue(xs, enter) |> balance

    let enqueue x (Queue(exit, enter)) =
        match exit with
        | [] -> Queue([x], [])
        | _  -> Queue(exit, x :: enter)

    let pop queue = (peek queue, remove queue)

    let tryPop (Queue(exit, enter) as queue) =
        match exit with
        | [] -> None
        | _  -> pop queue |> Some

    let singleton x = Queue([x], [])

    let ofList list = List.fold (Func.flip enqueue) empty list

    let isEmpty (Queue(exit, enter)) =
        match exit with
        | [] -> true
        | _  -> false