namespace FSharpExt

module Seq =
    let contains x seq = Seq.exists ((=) x) seq