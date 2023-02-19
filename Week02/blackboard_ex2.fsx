let rec unzip xys =
    match xys with
    | [] -> ([],[])
    | (x,y)::tail -> let (xs,ys) = unzip tail in (x::xs,y::ys);;
unzip [(1,2);(3,4);(5,6)];;