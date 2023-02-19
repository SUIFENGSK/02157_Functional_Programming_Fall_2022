let rec zip xs =
    match xs with
    |([xs],[]) -> []
    |([],[xs]) -> []
    |(x::a,y::b) -> (x,y)::zip (a,b)
    | _ -> [];;
zip ([1;2;3;4],[5;6;7;8]);;