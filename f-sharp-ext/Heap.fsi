namespace FSharpExt

type Heap<'a, 'b when 'a : comparison>

module Heap =
    val insert : 'a * 'b -> Heap<'a, 'b> -> Heap<'a, 'b>
    val min : Heap<'a, 'b> -> 'a * 'b
    val remove : Heap<'a, 'b> -> Heap<'a, 'b>
    val pop : Heap<'a, 'b> -> ('a * 'b) * Heap<'a, 'b>
    val tryPop : Heap<'a, 'b> -> (('a * 'b) * Heap<'a, 'b>) option
    val singleton : 'a * 'b -> Heap<'a, 'b>
    val empty : Heap<'a, 'b>
    val ofList : ('a * 'b) list -> Heap<'a, 'b>
    val ofValues : 'b list -> ('b -> 'a) -> Heap<'a, 'b>
    val sort : 'a list -> 'a list when 'a : comparison
    val isEmpty : Heap<'a, 'b> -> bool
