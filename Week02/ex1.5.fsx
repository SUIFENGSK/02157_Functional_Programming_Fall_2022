// Fibonacci numbers
let rec F n= 
    match n with
    | 0 -> 0
    | 1 -> 1
    | n -> F(n-1)+F(n-2);;
F(4)