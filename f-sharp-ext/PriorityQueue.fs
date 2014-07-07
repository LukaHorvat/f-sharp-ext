namespace FSharpExt

type PriorityQueue<'a, 'b when 'b : comparison> = PriorityQueue of Heap<'b, 'a> * ('a -> 'b)

module PriorityQueue =
    let empty = PriorityQueue(Heap.empty, id)
    let singleton x = PriorityQueue(Heap.singleton (x, x), id)
    let singletonWithFunction x fn = PriorityQueue(Heap.singleton (fn x, x), fn)
    let ofFunction fn = PriorityQueue(Heap.empty, fn)
    let remove (PriorityQueue(heap, fn)) = PriorityQueue(Heap.remove heap, fn)
    let peek (PriorityQueue(heap, fn)) = Heap.min heap |> snd
    let enqueue x (PriorityQueue(heap, fn)) = PriorityQueue(Heap.insert (fn x, x) heap, fn)
    let dequeue queue = (peek queue, remove queue)
    let isEmpty (PriorityQueue(heap, _)) = Heap.isEmpty heap
    let ofList list = PriorityQueue(Heap.ofValues list id, id)
    let ofFunctionList fn list = PriorityQueue(Heap.ofValues list fn, fn)