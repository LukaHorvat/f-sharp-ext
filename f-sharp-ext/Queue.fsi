namespace FSharpExt

type Queue<'a>

module Queue =
    val empty : Queue<'a>
    val singleton : 'a -> Queue<'a>
    val enqueue : 'a -> Queue<'a> -> Queue<'a>
    val peek : Queue<'a> -> 'a
    val remove : Queue<'a> -> Queue<'a>
    val pop : Queue<'a> -> 'a * Queue<'a>
    val isEmpty : Queue<'a> -> bool
    val tryPop : Queue<'a> -> ('a * Queue<'a>) option
    val ofList : 'a list -> Queue<'a>