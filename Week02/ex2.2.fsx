// pow : string * int -> string
let rec pow (s, n) =
    match n with
    | 0 -> ""
    | n -> s + pow(s, n-1)
pow("a",3);