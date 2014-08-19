#nowarn "25"

namespace FSharpExt

type Zipper<'a, 'b> = {
    getChildren : 'a -> 'a list
    reconstruct : 'a -> 'a -> int -> 'a
    getValue : 'a -> 'b
    setValue : 'b -> 'a -> 'a
    current : 'a
    behind : ('a * int) list
}

module Zipper =
    let ofCollection children cons get set collection = { 
        getChildren = children
        reconstruct = cons
        getValue = get
        setValue = set
        current = collection
        behind = [] 
    }
    let moveForward n zipper = { 
        zipper with 
            current = zipper.getChildren zipper.current |> (Func.flip List.nth) n
            behind = (zipper.current, n) :: zipper.behind    
    }
    let moveBackward ({ current = current; behind = (last, dir) :: rest } as zipper) = {
        zipper with
            current = zipper.reconstruct last current dir
            behind = rest
    }
    let rec isTop zipper =
        match zipper with
        | { behind = [] } -> true
        | _               -> false
    let rec toRoot zipper =
        match isTop zipper with
        | true  -> zipper
        | false -> moveBackward zipper |> toRoot
    let extract zipper = zipper.current
    let toCollection zipper = toRoot zipper |> extract
    let editCurrent x zipper = { zipper with current = zipper.setValue x zipper.current }
    let insert node dir zipper = { 
        zipper with 
            current = zipper.reconstruct node zipper.current dir
    }
    let append node dir zipper = {
        zipper with
            current = node
            behind = (zipper.reconstruct zipper.current node dir, dir) :: zipper.behind
    }

type ListZipper<'a> = Zipper<'a list, 'a>

module ListZipper =
    let ofList list = 
        Zipper.ofCollection
            (List.tail >> List.singleton)
            (fun (x :: xs) ys _ -> x :: ys)
            List.head
            (fun x (y :: ys) -> x :: ys)
            list
    let moveRight (zipper : ListZipper<'a>) = Zipper.moveForward 0 zipper
    let moveLeft (zipper : ListZipper<'a>) = Zipper.moveBackward zipper
    let toRoot (zipper : ListZipper<'a>) = Zipper.toRoot zipper
    let toList (zipper : ListZipper<'a>) = Zipper.extract zipper
    let modify x (zipper : ListZipper<'a>) = Zipper.editCurrent x zipper
    let insert x (zipper : ListZipper<'a>) = Zipper.insert [x] 0 zipper
    let append x (zipper : ListZipper<'a>) = Zipper.append [x] 0 zipper
    let isHead (zipper : ListZipper<'a>) = Zipper.isTop zipper
