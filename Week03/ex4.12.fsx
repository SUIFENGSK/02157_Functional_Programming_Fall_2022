let rec sum p xs =
    match xs with
    | [] -> 0
    | x::xs -> if p x then x + sum p xs else sum p xs;;
let p = fun x -> x > 0;;
sum p [-1;2;3;4];;
