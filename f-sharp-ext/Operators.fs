namespace FSharpExt


module Instance = 
    let inline instance (a : ^a) = ((((^a : (static member instance : ^a -> _) a))))
    let inline instance2 (a : ^a, b : ^b) = (((((^a or ^b) : (static member instance : ^a * ^b -> _) (a, b)))))
    let inline instance3 (a : ^a, b : ^b, c : ^c) = 
        (((((^a or ^b or ^c) : (static member instance : ^a * ^b * ^c -> _) (a, b, c)))))
    let inline instance4 (a : ^a, b : ^b, c : ^c, d : ^d) = 
        (((((^a or ^b or ^c or ^d) : (static member instance : ^a * ^b * ^c * ^d -> _) (a, b, c, d)))))
    let inline instance5 (a : ^a, b : ^b, c : ^c, d : ^d, e : ^e) = 
        (((((^a or ^b or ^c or ^d or ^e) : (static member instance : ^a * ^b * ^c * ^d * ^e -> _) (a, b, c, d, e)))))

open Instance

[<RequireQualifiedAccess>]
type Inline = 
    | Inline
    static member inline instance() = fun (x : 'x) -> instance (Unchecked.defaultof<'r>) x : 'r
    static member inline instance (a : 'a) = fun (x : 'x) -> instance2 (a, Unchecked.defaultof<'r>) x : 'r
    static member inline instance (a : 'a, b : 'b) = fun (x : 'x) -> instance3 (a, b, Unchecked.defaultof<'r>) x : 'r
    static member inline instance (a : 'a, b : 'b, c : 'c) = 
        fun (x : 'x) -> instance4 (a, b, c, Unchecked.defaultof<'r>) x : 'r
    static member inline instance (a : 'a, b : 'b, c : 'c, d : 'd) = 
        fun (x : 'x) -> instance5 (a, b, c, d, Unchecked.defaultof<'r>) x : 'r
        
module Monad =
    [<RequireQualifiedAccess>]
    type Bind =
        | Bind
        static member instance (Bind, m : 'a option, _ : 'b option) = fun (f : 'a -> 'b option) -> Option.bind f m
        static member instance (Bind, m : 'a list, _ : 'b list) = fun (f : 'a -> 'b list) -> List.collect f m
        
    [<RequireQualifiedAccess>]
    type Return = 
        | Return
        static member instance (Return, _ : 'a option) = fun (x : 'a) -> Some x
        static member instance (Return, _ : 'a list) = fun (x : 'a) -> List.singleton x
        
module StringExt =
    [<RequireQualifiedAccess>]
    type StringEq = 
        | StringEq
        static member instance (StringEq, str : string, cl : char list, _ : bool) = fun () -> List.ofSeq str = cl
        static member instance (StringEq, cl : char list, str : string, _ : bool) = fun () -> List.ofSeq str = cl
        static member instance (StringEq, a : string, b : string, _ : bool) = fun () -> a = b
        static member instance (StringEq, a : char list, b : char list, _ : bool) = fun () -> a = b

    [<RequireQualifiedAccess>]        
    type StringConvert =
        | StringConvert
        static member instance (StringConvert, str : string, _) = fun () -> List.ofSeq str
        static member instance (StringConvert, cl : char list, _) = fun () -> System.String.Concat cl
        
    [<RequireQualifiedAccess>]
    type StringAdapter =
        | StringAdapter
        static member instance (StringAdapter, i : string, f : string -> string, _ : string) = fun () -> i |> f
        static member instance (StringAdapter, i : string, f : string -> string, _ : char list) = fun () -> i |> f |> List.ofSeq
        static member instance (StringAdapter, i : string, f : string -> char list, _ : string) = fun () -> i |> f |> System.String.Concat
        static member instance (StringAdapter, i : string, f : string -> char list, _ : char list) = fun () -> i |> f
        static member instance (StringAdapter, i : string, f : char list -> string, _ : string) = fun () -> i |> List.ofSeq |> f
        static member instance (StringAdapter, i : string, f : char list -> string, _ : char list) = fun () -> i |> List.ofSeq |> f |> List.ofSeq
        static member instance (StringAdapter, i : string, f : char list -> char list, _ : string) = fun () -> i |> List.ofSeq |> f |> System.String.Concat
        static member instance (StringAdapter, i : string, f : char list -> char list, _ : char list) = fun () -> i |> List.ofSeq |> f
        static member instance (StringAdapter, i : char list, f : string -> string, _ : string) = fun () -> i |> System.String.Concat |> f
        static member instance (StringAdapter, i : char list, f : string -> string, _ : char list) = fun () -> i |> System.String.Concat |> f |> List.ofSeq
        static member instance (StringAdapter, i : char list, f : string -> char list, _ : string) = fun () -> i |> System.String.Concat |> f |> System.String.Concat
        static member instance (StringAdapter, i : char list, f : string -> char list, _ : char list) = fun () -> i |> System.String.Concat |> f
        static member instance (StringAdapter, i : char list, f : char list -> string, _ : string) = fun () -> i |> f
        static member instance (StringAdapter, i : char list, f : char list -> string, _ : char list) = fun () -> i |> f |> List.ofSeq
        static member instance (StringAdapter, i : char list, f : char list -> char list, _ : string) = fun () -> i |> f |> System.String.Concat
        static member instance (StringAdapter, i : char list, f : char list -> char list, _ : char list) = fun () -> i |> f
        
    /// <summary>
    /// Takes a function that takes either a string or a char list and returns either a string or a char list.
    /// Returns a function that does the same thing but can take both a string and a char list and return both, depending on the usage.
    /// </summary>
    /// <param name="f"></param>
    /// <param name="x"></param>
    let inline adapt (f : 'charlistOrString1 -> 'charlistOrString2) (x : 'charlistOrString3) : 'charlistoOrString4 = Inline.instance(StringAdapter.StringAdapter, x, f) ()

[<AutoOpen>]
module Operators = 
    /// <summary>
    /// Bind operator for monad m and function f. m must be one of the supported monads.
    /// m : 'a 'm
    /// f : 'a -> 'b 'm
    /// </summary>
    /// <param name="m">Must be one of the supported monads ('a 'm)</param>
    /// <param name="f">Functiom of type 'a -> 'b 'm</param>
    let inline (>>=) (m : 'ma) (f:'b -> 'mb) : 'mb = Inline.instance(Monad.Bind, m) f
    
    /// <summary>
    /// Lifts the value into the monad. Monad type is inferred from usage.
    /// </summary>
    /// <param name="x">Value to be lifted.</param>
    let inline return' (x : 'a) : 'ma = Inline.instance Monad.Return x
    
    /// <summary>
    /// Compares a two objects that can have the type of string or char list
    /// </summary>
    /// <param name="a"></param>
    /// <param name="b"></param>
    let inline (=!=) (a : 'charlistOrString1) (b : 'charlistOrString2) = Inline.instance(StringExt.StringEq, a, b) ()
    
    /// <summary>
    /// Converts the string to a char list and a char list to a string
    /// </summary>
    /// <param name="a"></param>
    let inline (!) (a : 'charlistOrString1) : 'charlistOrString2 = Inline.instance(StringExt.StringConvert, a) ()
    let (|>|) = Parse.(|>|)
    let (|||) = Parse.(|||)
    let (|++|) = Parse.(|++|)
    let (|.>|) = StateParse.(|.>|)
    let (|.||) = StateParse.(|.||)