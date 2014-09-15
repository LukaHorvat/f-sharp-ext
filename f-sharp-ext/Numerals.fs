namespace FSharpExt

type Zero = 
    | Zero
    static member (+) (Zero, a) = a
    static member (!!) Zero = Zero
    static member (=<=) (Zero, list) = 
        match list with 
        | [] -> []
        | _ -> invalidOp "List is too long"
    static member (!!!) Zero = 0 

and Succ<'a> = 
    | Succ of 'a
    static member inline (+) (Succ a, b) = Succ(a + b)
    static member inline (!!) (Succ a) = Succ(!!a)
    static member inline (=<=) (Succ a, list) = 
        match list with
        | x :: xs -> x :: (a =<= xs)
        | _ -> invalidOp "List is too short"
    static member inline (!!!) (Succ a) = 1 + !!!a

type One = Zero Succ

type Two = One Succ

type Three = Two Succ

type Four = Three Succ

type ProperNum = 
    | ProperNum
    static member instance (ProperNum, ProperNum) = fun () -> ()
    static member instance (ProperNum, _ : Zero) = fun () -> Zero
    static member inline instance (ProperNum, _ : Succ<'a>) = fun () -> Succ(Inline.instance ProperNum ())

module Numerals = 
    let inline instance() = Inline.instance ProperNum ()
    let inline checkListLength (list : 'a list) (num : 'b) : 'a list = num =<= list
    let inline showPlain n : string = (!!!n).ToString()
    let inline show n : string = showPlain n + "T"