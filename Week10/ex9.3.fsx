// tail recursion of ex1.6
let rec sumA = 
    function
    | (m,0,s) -> s+m
    | (m,n,s) -> sumA (m,n-1,s+m+n)

sumA (4,1,0)