namespace FSharpExt

module Func = 
    /// <summary>
    /// Reverses the order of the functions first two arguments
    /// </summary>
    /// <param name="f"></param>
    /// <param name="x"></param>
    /// <param name="y"></param>
    let flip f x y = f y x

    /// <summary>
    /// Reorders the first three arguments of a function from a -> b -> c to a -> c -> b
    /// </summary>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <param name="c"></param>
    let reorderAcb f a c b = f a b c

    /// <summary>
    /// Reorders the first three arguments of a function from a -> b -> c to b -> a -> c
    /// </summary>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <param name="c"></param>
    let reorderBac f b a c = f a b c

    /// <summary>
    /// Reorders the first three arguments of a function from a -> b -> c to b -> c -> a
    /// </summary>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <param name="c"></param>
    let reorderBca f b c a = f a b c

    /// <summary>
    /// Reorders the first three arguments of a function from a -> b -> c to c -> a -> b
    /// </summary>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <param name="c"></param>
    let reorderCab f c a b = f a b c

    /// <summary>
    /// Reorders the first three arguments of a function from a -> b -> c to c -> b -> a
    /// </summary>
    /// <param name="f"></param>
    /// <param name="a"></param>
    /// <param name="b"></param>
    /// <param name="c"></param>
    let reorderCba f c b a = f a b c

    let uncurry f (x, y) = f x y
    let uncurry3 f (x, y, z) = f x y z
    let uncurry4 f (x, y, z, w) = f x y z w
    let curry f x y = f (x, y)
    let curry3 f x y z = f (x, y, z)
    let curry4 f x y z w = f (x, y, z, w)
    let tuple x y = curry id x y
    let tuple3 x y z = curry3 id x y z
    let tuple4 x y z w = curry4 id x y z w
    let combineWith combiner f g x = combiner (f x) (g x)
    let combineTuple f g x = (f x, g x)
    let inline combinePlus f g x = f x + g x
    let inline combineMinus f g x = f x - g x
    let inline combineTimes f g x = f x * g x
    let inline combineDiv f g x = f x / g x
    /// <summary>
    /// Takes a condition functions and a true and false branch functions. If the argument applied to the condition is true,
    /// it applies it to the first function and returns the result. Otherwise, it applies it to the second function.
    /// </summary>
    /// <param name="cond"></param>
    /// <param name="f"></param>
    /// <param name="g"></param>
    /// <param name="x"></param>
    let combineIf cond f g x = 
        if cond x then f x
        else g x
    /// <summary>
    /// Given a parameter x, it produces a function that returns x no mather what the argument is
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    let constFunc x _ = x
