namespace FSharpExt

open System.Collections.Generic

type Zipper<'a, 'b>

module Zipper =
    val ofCollection : ('a -> 'a list) -> ('a -> 'a -> int -> 'a) -> ('a -> 'b) -> ('b -> 'a -> 'a) -> 'a -> Zipper<'a, 'b>
    val moveForward : int -> Zipper<'a, 'b> -> Zipper<'a, 'b>
    val moveBackward : Zipper<'a, 'b> -> Zipper<'a, 'b>
    val editCurrent : 'b -> Zipper<'a, 'b> -> Zipper<'a, 'b>
    val toRoot : Zipper<'a, 'b> -> Zipper<'a, 'b>
    val toCollection : Zipper<'a, 'b> -> 'a
    val isTop : Zipper<'a, 'b> -> bool
    
type ListZipper<'a> = Zipper<'a list, 'a>

module ListZipper =
    val ofList : 'a list -> ListZipper<'a>
    val moveRight : ListZipper<'a> -> ListZipper<'a>
    val moveLeft : ListZipper<'a> -> ListZipper<'a>
    val toRoot : ListZipper<'a> -> ListZipper<'a>
    val toList : ListZipper<'a> -> 'a list
    val modify : 'a -> ListZipper<'a> -> ListZipper<'a>
    val insert : 'a -> ListZipper<'a> -> ListZipper<'a>
    val append : 'a -> ListZipper<'a> -> ListZipper<'a>
    val isHead : ListZipper<'a> -> bool