let rec evenN n=
    match n with
    | 0 -> []
    | _ when n%2=0 -> evenN (n-1) @ [n]
    | _ -> evenN (n-1);;

evenN 10;;