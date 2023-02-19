// Problem 1
let rec f1 a b = if a > b then f1 (a-b) b else a;;
let rec f2 a b = if a > b then 1 + f2 (a-b) b else 0;;

// q1
// f1 23 3
// 2
// f2 23 3
// 7

// q2
// f1 int -> int -> int
// f2 int -> int -> int

// q4
let rec f2A a b acc = if a>b then f2A (a-b) b 1+acc else acc
// f2A 23 3 0

// Problem 2
type Species = string
type Location = string
type Time = int
type Observation = Species * Location * Time
let os = [("Owl","L1",3); ("Sparrow","L2",4); ("Eagle","L3",5);("Falcon","L2",7); ("Sparrow","L1",9); ("Eagle","L1",14)]

// q1
let rec locationsOf s os =
    match os with
    | [] -> []
    | (s1,l1,t1)::xs -> if s=s1 then l1::(locationsOf s xs) else locationsOf s xs

// locationsOf "Sparrow" os

// q2
let rec extractAFromOCC occ =
    match occ with
    | [] -> []
    | (a1,_)::xs -> a1::extractAFromOCC xs

let rec insertHelp a occ =
    match occ with
    | [] -> []
    | (a1,c1)::xs -> if a1=a then (a1,c1+1)::(insertHelp a xs)
                     else (a1,c1)::(insertHelp a xs)

let insert a occ = if List.contains a (extractAFromOCC occ)
                   then insertHelp a occ
                   else List.append occ [(a,1)]

// let occ = [("test"),1]
// insert "test1" occ
// insert 'a -> list<'a * int> -> list<'a * int>

// q3
let rec extractAFromOs os =
    match os with
    | [] -> []
    | (a1,_,_)::xs -> a1::extractAFromOs xs

let toCount os = let AList = extractAFromOs os
                 List.countBy id AList

// toCount os

// q4
let rec extractOinIntv intv os =
    match os with
    | [] -> []
    | (s1,l1,t1)::xs -> if t1<=snd intv && t1>=fst intv 
                        then (s1,l1,t1)::extractOinIntv intv xs
                        else extractOinIntv intv xs

let select f intv os = let olist= extractOinIntv intv os
                       List.map (fun x -> f x) olist

// q5
// let f x = let (s,l,t) = x 
//           (s,l)

// select f (4,9) os

// Problem 3
let rec f g = function
    | (x::xs,y::ys) -> g x y || f g (x::xs,ys) || f g (xs,y::ys)
    | _ -> false

let h z = f (>) z;;

// f : 'a -> 'b -> bool
// h : 'a list * 'b list -> bool
// f (<) ([1;2;3],[4;5;6])
// h ([1;2;3],[4;5;6])

// Problem 4
type K<'a> = L | M of K<'a> * 'a * K<'a>
let rec c x =
    match x with
    | L -> []
    | M(y,v,w) -> c y @ [v] @ c w

let rec d i = function
    | L -> (L,i)
    | M(x,y,z) -> let (x1,j) = d i x
                  let (z1,k) = d (j+1) z
                  (M(x1,(y,j),z1),k)

// q1
let value1 = M(L,("a",[1]),L)
let value2 = L
let value3 = M(M(L,("b",[1]),L),("a",[1]),L)
let value4 = M(L,("a",[1]),M(L,("b",[1]),L))

// q2
// c: K<'a> -> 'a list
// d: int -> K<'a> -> K<'a> * int

// q3
// c value1
// d 0 value1
// c computes the inorder traversal of the tree, and returns the list of values.
// d 0 computes the index of each node in the tree, starting from 0, and returns the tree with the index.


// Problem 5
type Term = | V of string | C of int | F of string * Term list
let term1 = V "x"
let term2 = C 3 
let term3 = F("f0",[])
let term4 = F("f1",[C 3;F("f0",[])])
let term5 = F("max",[V "x";C 3])

// q1
let rec isGround term =
    match term with
    | V v -> true
    | C c -> false
    | F(_,ts) -> isGroundHelp ts
and isGroundHelp ts =
        match ts with
        | [] -> false
        | t::xs -> isGround t || isGroundHelp xs

// isGround term5

// q2
let rec toString term =
    match term with
    | V v -> v
    | C c -> string c
    | F(f,ts) ->  f+"(" + toStringHelp ts + ")"
and toStringHelp ts =
    match ts with
    | [] -> ""
    | t::[] -> toString t
    | t::xs -> toString t + "," + toStringHelp xs

let t6 = F("f3",[F("f2",[C 1; C 2]); F("f1",[V "x"]); F("f0",[])]);;
// toString t6

// q3
let rec subst x t' t =
    match t with
    | V v -> if v=x then V t' else V v
    | C c -> C c
    | F(f,ts) -> F(f,substHelp x t' ts)
and substHelp x t' ts =
    match ts with
    | [] -> []
    | t::xs -> subst x t' t :: substHelp x t' xs

// subst "x" "y" term5

// q4
// Declare a function extractArities: Term -> Map<string,int> option. The value
// of extractArities t is None when t is an illegal term. Otherwise extractArities t =
// Some m, where m is a map. The keys of m are the function symbols occurring in t and
// (f, n) is a entry of m, when f is used with arity n in t. For example, extractArities t6
// gives Some m, where m contains 4 entries ("f0",0), ("f1",1), ("f2",2) and ("f3",3).

let rec extractArities t =
    match t with
    | V v -> Some Map.empty
    | C c -> Some Map.empty
    | F(f,ts) -> if List.length ts = 0 then Some (Map.add f 0 Map.empty)
                 else let m = extractAritiesHelp ts
                      match m with
                      | None -> None
                      | Some m1 -> Some (Map.add f (List.length ts) m1)
and extractAritiesHelp ts =
    match ts with
    | [] -> Some Map.empty
    | t::xs -> let m = extractArities t
               match m with
               | None -> None
               | Some m1 -> let m2 = extractAritiesHelp xs
                            match m2 with
                            | None -> None
                            | Some m3 -> Some (Map.fold (fun acc key value -> Map.add key value acc) m1 m3)

extractArities t6
                