let rec split xs =
    match xs with
    | [] -> ([],[])
    | [xs] -> ([xs],[])
    | x::y::xs -> let (a,b) = split xs in (x::a,y::b);;
split [1;2;3;4];;