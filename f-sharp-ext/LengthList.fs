namespace FSharpExt

type LengthList<'innerType, 'length> = 
    | LengthList of 'innerType list * 'length
    static member inline (+) (LengthList(list1, len) : LengthList<'a, 'l>, LengthList(list2, _) : LengthList<'b, 'l>) = 
        LengthList(List.map2 (+) list1 list2, len)
    static member cons (x : 'a, LengthList(list, len) : LengthList<'a, _>) = LengthList(x :: list, Succ len)
    static member head (LengthList(x :: _, Succ _)) = x
    static member tail (LengthList(_ :: xs, Succ len)) = LengthList(xs, len)
    static member inline (!!) (LengthList(_, a)) = !!a

module LengthList = 
    let empty = LengthList([], Zero)
    let inline dot (LengthList(l1, _) : LengthList<'a, 'len>) (LengthList(l2, _) : LengthList<'b, 'len>) = 
        List.map2 (*) l1 l2 |> List.sum
    let inline ofList (list : 'a list) (len : 'b) : LengthList<'a, 'b> = 
        LengthList(Numerals.checkListLength list len, len)
    let inline showPlain (LengthList(list, _)) : string = List.map (fun x -> x.ToString()) list |> String.concat " "
    let inline show (LengthList(_, len) as ll) : string = "[" + (Numerals.show len) + " " + showPlain ll + "]"
