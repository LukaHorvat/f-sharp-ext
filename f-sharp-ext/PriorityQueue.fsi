namespace FSharpExt

type PriorityQueue<'a, 'b when 'b : comparison>

module PriorityQueue =
    val isEmpty : PriorityQueue<'a, 'b> -> bool
    val empty : PriorityQueue<'a, 'a>
    val singleton : 'a -> PriorityQueue<'a, 'a>
    val singletonWithFunction : 'a -> ('a -> 'b) -> PriorityQueue<'a, 'b>
    val ofFunction : ('a -> 'b) -> PriorityQueue<'a, 'b>
    val enqueue : 'a -> PriorityQueue<'a, 'b> -> PriorityQueue<'a, 'b>
    val dequeue : PriorityQueue<'a, 'b> -> 'a * PriorityQueue<'a, 'b>
    val peek : PriorityQueue<'a, 'b> -> 'a
    val remove : PriorityQueue<'a, 'b> -> PriorityQueue<'a, 'b>
    val ofList : 'a list -> PriorityQueue<'a, 'a>
    val ofFunctionList : ('a -> 'b) -> 'a list -> PriorityQueue<'a, 'b>