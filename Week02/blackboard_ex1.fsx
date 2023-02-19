let h x (s,p) = (s+x,p*x);;

let rec sumProd xs=
    match xs with
    | [] -> (0,1)
    | x::tail -> h x (sumProd tail);;

sumProd [1;2;4];;