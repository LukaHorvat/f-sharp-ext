﻿#nowarn "25"

namespace FSharpExt

type HeapNode<'a, 'b when 'a : comparison> = 
    | Full of 'a * 'b * HeapNode<'a, 'b> * HeapNode<'a, 'b>
    | Half of 'a * 'b * HeapNode<'a, 'b>
    | Leaf of 'a * 'b
    | Empty
type Direction = Left | Right
type Pointer = Direction list
type HeapZipper<'a, 'b when 'a : comparison> = HeapZipper of HeapNode<'a, 'b> * (HeapNode<'a, 'b> * Direction) list
type Heap<'a, 'b when 'a : comparison> = Heap of HeapZipper<'a, 'b> * Pointer

module Heap =    
    let (|KeyValue|) zipper =
        match zipper with
        | Full(k, v, _, _) | Half(k, v, _) | Leaf(k, v) -> (k, v)
        | Empty -> failwith "List is empty"

    let cut node =
        match node with
        | Leaf(k, v)          -> Empty
        | Half(k, v, _)       -> Leaf(k, v)
        | Full(k, v, left, _) -> Half(k, v, left)

    let rec next pointer =
        match pointer with
        | []                    -> [Left]
        | x :: xs when x = Left -> Right :: xs
        | x :: xs               -> Left :: next xs

    let rec previous pointer =
        match pointer with
        | [Left]                 -> []
        | x :: xs when x = Right -> Left :: xs
        | x :: xs                -> Right :: previous xs

    let moveLeftZipper (HeapZipper((Full(_, _, left, _) | Half(_, _, left)) as node, path)) = HeapZipper(left, (node, Left) :: path)

    let moveRightZipper (HeapZipper(Full(_, _, _, right) as node, path)) = HeapZipper(right, (node, Right) :: path)

    let moveDirectionZipper direction zipper =
        match direction with
        | Left  -> moveLeftZipper zipper
        | Right -> moveRightZipper zipper

    let moveAlongPathZipper path zipper = List.fold (Func.flip moveDirectionZipper) zipper (List.rev path)

    let moveUpZipper (HeapZipper(current, (last, dir) :: path)) =
        match last, dir with
        | Full(k, v, _, right), Left -> HeapZipper(Full(k, v, current, right), path)
        | Half(k, v, _), Left        -> HeapZipper(Half(k, v, current), path)
        | Full(k, v, left, _), Right -> HeapZipper(Full(k, v, left, current), path)

    let rec toRootZipper (HeapZipper(current, path) as zipper) =
        match path with
        | []      -> zipper
        | x :: xs -> zipper |> moveUpZipper |> toRootZipper   
    
    let keyValue (HeapZipper(KeyValue(k, v), _)) = (k, v)

    let modifyCurrentZipper (k, v) (HeapZipper(current, path)) =
        match current with
        | Full(_, _, left, right) -> HeapZipper(Full(k, v, left, right), path)
        | Half(_, _, left)        -> HeapZipper(Half(k, v, left), path)
        | _                       -> HeapZipper(Leaf(k, v), path)

    let appendLeafZipper (k, v) (HeapZipper(current, path)) =
        match current with
        | Half(ck, cv, left) -> HeapZipper(Full(ck, cv, left, Leaf(k, v)), path) |> moveRightZipper
        | Leaf(ck, cv)       -> HeapZipper(Half(ck, cv, Leaf(k, v)), path) |> moveLeftZipper
        | Empty              -> HeapZipper(Leaf(k, v), [])

    let removeLeafZipper zipper =
        match zipper with
        | HeapZipper(_, (node, dir) :: path) -> HeapZipper(cut node, path)
        | _                              -> HeapZipper(Empty, [])

    let rec bubbleUpZipper (HeapZipper(KeyValue(k, v), path) as zipper) =
        match path with
        | (KeyValue(pk, pv), _) :: rest when pk > k -> modifyCurrentZipper (pk, pv) zipper |> moveUpZipper |> modifyCurrentZipper (k, v) |> bubbleUpZipper
        | _                                         -> zipper 

    let rec bubbleDownZipper (HeapZipper(current, _) as zipper) =
        let inline move fn (ak, av) (bk, bv) = modifyCurrentZipper (bk, bv) zipper |> fn |> modifyCurrentZipper (ak, av) |> bubbleDownZipper
        let inline right x y = move moveRightZipper x y
        let inline left x y = move moveLeftZipper x y
        match current with
        | Full(k, v, KeyValue(lk, lv), KeyValue(rk, rv)) when k > lk || k > rk -> if lk > rk then right (k, v) (rk, rv) else left (k, v) (lk, lv)
        | Half(k, v, KeyValue(lk, lv)) when k > lk                             -> left (k, v) (lk, lv)
        | _                                                                    -> zipper

    let toRoot (zipper, pointer) = (toRootZipper zipper, pointer)
    let appendLeaf (k, v) (zipper, pointer) =
        match pointer with
        | []      -> appendLeafZipper (k, v) zipper, next pointer
        | x :: xs -> (moveAlongPathZipper xs zipper |> appendLeafZipper (k, v), next pointer)
    let removeLeaf (zipper, pointer) = (moveAlongPathZipper (previous pointer) zipper |> removeLeafZipper, previous pointer)
    let bubbleUp (zipper, pointer) = (bubbleUpZipper zipper, pointer)
    let bubbleDown (zipper, pointer) = (bubbleDownZipper zipper, pointer)

    let (|Root|) (Heap(HeapZipper(root, _), _)) = root

    let insert (k, v) (Heap(zipper, pointer)) = appendLeaf (k, v) (zipper, pointer) |> bubbleUp |> toRoot |> Heap

    let min (Heap(zipper, pointer)) = keyValue zipper

    let remove (Heap(zipper, pointer)) = 
        (modifyCurrentZipper (moveAlongPathZipper (previous pointer) zipper |> keyValue) zipper, pointer) 
        |> removeLeaf 
        |> toRoot 
        |> bubbleDown 
        |> toRoot 
        |> Heap

    let pop heap = (min heap, remove heap)

    let tryPop heap =
        match heap with
        | Root(Empty) -> None
        | _           -> pop heap |> Some

    let singleton (k, v) = Heap(HeapZipper(Leaf(k, v), []), [Left])

    let empty = Heap(HeapZipper(Empty, []), [])

    let ofList list = 
        match list with
        | []            -> empty
        | first :: tail -> List.fold (fun h x -> insert x h) (singleton first) tail

    let ofValues list fn = List.map (fun x -> (fn x, x)) list |> ofList

    let sort list = ofValues list id |> Seq.unfold tryPop |> Seq.map snd |> List.ofSeq

    let isEmpty (Heap(HeapZipper(current, _), _)) =
        match current with
        | Empty -> true
        | _     -> false

    let debugPrint (Heap(HeapZipper(current, path), pointer)) =
        let depth = List.length pointer
        let printNode (KeyValue(k, v)) level =
            let txt = sprintf "(%d, %d)" k v
            let room = (10 * pown 2 (depth - level) - txt.Length)
            let halfRoom = room / 2
            let spacingLeft = String.replicate halfRoom " "
            let spacingRight = String.replicate (room - halfRoom) " "
            printf "%s" (spacingLeft + txt + spacingRight)
        let rec bfs node level =
            match node with
            | Leaf(_, _)              -> seq { yield (node, level) }
            | Half(_, _, left)        -> seq { yield (node, level); yield! bfs left (level + 1) }
            | Full(_, _, left, right) -> seq { yield (node, level); yield! bfs left (level + 1); yield! bfs right (level + 1) }
        Seq.groupBy (fun (n, l) -> l) (bfs current 0) |> Seq.iter (fun (_, s) -> Seq.iter (fun (n, l) -> printNode n l) s; printf "\n")