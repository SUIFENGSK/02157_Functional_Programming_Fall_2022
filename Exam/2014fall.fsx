// Problem 2
// q1
let multTale n = seq {for i in [1..10] do yield n*i}
// multTale 3

// q2
let tableOf m n f = seq {for i in [1..m] do for j in [1..n] do yield (i,j,f i j)}
// tableOf 3 4 (+)

// q3
let aSeq = Seq.initInfinite (fun i -> String.replicate (i+1) "a")
// aSeq

let rec f i = function
    | [] -> []
    | x::xs -> (x+i)::f (i*i) xs;;
// q4
// x+i, x+i^2, x+i^4...
//f adds i^(2^index) to each element in the input list.
f 2 [0;0;0;0;0]

// q5
let rec fA i a = function
    | [] -> List.rev a
    | x::xs -> fA (i*i) ((x+i)::a) xs
fA 2 [] [0;0;0;0;0]

let rec fC i c = function
    | [] -> c []
    | x::xs -> fC (i*i) (fun v -> c ((x+i)::v)) xs

fC 2 id [0;0;0;0;0]