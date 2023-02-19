// problem 1

// question 1
let rec f n = function 
    | 0 -> 1
    | k when k>0 -> n * (f n (k-1))
    | _ -> failwith "illegal argument";;

// f(n,k)=n^k
// f 5 3

let rec g p f = function
    | [] -> []
    | x::xs when p x -> f x :: g p f xs
    | _::xs -> g p f xs;;

// when p x true -> f(x)->[]
// g (fun x -> x>5) (fun x -> x) [1;2;3;4;5;6;7;8;9;10]

type T = | A of int
         | B of string
         | C of T*T;;
let rec h = function
    | A n -> string n
    | B s -> s
    | C(t1,t2) -> h t1 + h t2;;

// combine with two strings
// h (C(A 6, B "C"))

// question 2
// f: int -> int -> int
// g: ('a -> bool) -> ('a -> 'b) -> 'a list -> 'b list
// h: T -> string

// question 3
// tail recursion
let rec fA n s = function
    | 0 -> s
    | k when k>0 -> (fA n (s*n) (k-1))
    | _ -> failwith "illegal argument";;

// fA 5 1 3

// continuation
let rec fC n c = function 
    | 0 -> c 1
    | k when k>0 -> (fC n (fun v -> c(v*n)) (k-1))
    | _ -> failwith "illegal argument";;

// fC 10 id 3

let sq = Seq.initInfinite (fun i -> 3*i);;
let k j = seq {for i in sq do yield (i,i-j) };;
let xs = Seq.toList (Seq.take 4 sq);;
let ys = Seq.toList (Seq.take 4 (k 2));;

// question 4
// sq: seq<int>
// sq -> [0;3;6;9...]
// k: int -> seq<int*int>
// k j -> [(0,0-j);(3,3-j);(6,6-j)...]

// question 5
// xs -> [0;3;6;9]
// ys -> [(0,-2);(3,1);(6,4);(9,7)]