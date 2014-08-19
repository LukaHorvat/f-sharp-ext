namespace FSharpExt

module Func =
    let flip f x y = f y x
    let uncurry f (x, y) = f x y
    let uncurry3 f (x, y, z) = f x y z
    let uncurry4 f (x, y, z, w) = f x y z w
    let curry f x y = f (x, y)
    let curry3 f x y z = f (x, y, z)
    let curry4 f x y z w = f (x, y, z, w)
    let tuple x y = curry id x y
    let tuple3 x y z = curry3 id x y z
    let tuple4 x y z w = curry4 id x y z w
    let combineTuple f g x = (f x, g x)
    let inline combinePlus f g x = f x + g x
    let inline combineMinus f g x = f x - g x
    let inline combineTimes f g x = f x * g x
    let inline combineDiv f g x = f x / g x
    let combineIf cond f g x = if cond x then f x else g x
    let constFunc x y = x