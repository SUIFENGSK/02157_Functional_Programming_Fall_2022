let rec bin n k =
    match (n,k) with
    | (n,0) -> 1
    | (n,k) when n=k -> 1
    | (n,k) -> bin(n-1)(k-1) + bin(n-1)(k);;
bin(4)(2)