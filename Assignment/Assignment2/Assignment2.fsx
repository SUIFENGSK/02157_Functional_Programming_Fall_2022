// 02157 Functional programming - Assignment 2
// Name: Shuokai Ma
// Student number: s214919
// Date: 23/11/2022

/////////////
//Problem 1//
/////////////

let rec f x = function
    | [] -> [] // C1
    | y::ys -> (x,y)::f x ys;; // C2

// Question 1
// Because in the case of C2, the function f is called recursively and it 
// creates a huge amount of pending operations, e.g. (1,1)::((1,2)::((1,3)::f 1 [4;5;6])) ......
// therefore, the declaration of f is not tail recursive.

// Question 2
let rec fA x accList = function
    | [] -> accList
    | y::ys -> fA x (accList @ [(x,y)]) ys;;

// fA 1 [] [1;2;3;4;5;6];;

// Question 3
let rec fC x k  = function
    | [] -> k []
    | y::ys -> fC x (fun z -> k ((x,y)::z)) ys;;

// fC 1 id [1;2;3;4;5;6];;

/////////////
//Problem 2//
/////////////

// Question 1
let rec f2 g (h1,h2) = function
    | [] -> [] // C1
    | x::xs when g x -> h1 x :: f2 g (h1,h2) xs // C2
    | x::xs -> h2 x :: f2 g (h1,h2) xs;; // C3

// The type of f is ('a -> bool) -> ('a -> 'b) * ('a -> 'b) -> 'a list -> 'b list.
// The f takes three arguments (g, (h1,h2) and a list) and computes a list.
// However, there are three different conditions. If the input list is empty, it will output also an empty list.
// But if elements from the input list satisfy the condition g ('a -> bool), they 
// will be applied by the function h1. Otherwise, they will be applied by the function f2. 
// After that, they will all be stored in the new list as output.

// Question 2
let fListMap g (h1,h2) ls= if (List.length ls) = 0 then []
                                     else List.map (fun x -> if g x then h1 x else h2 x) ls;;
// fListMap (fun x -> x > 3) ((fun x -> x + 1), (fun x -> x - 1)) [1;2;3;4;5;6];;

// Question 3
type A<'a> = 
    | D of 'a * bool
    | E of A<'a> * A<'a>

let rec g acc x = 
    match x with
    | E (y,z) -> g (g acc z) y // C1
    | D (a,true) -> a::acc // C2
    | _ -> acc;; // C3

let h x = g [] x;;

let value1= D(["a";"b"],true)
let value2= E(D(["a";"b"],true),D(["a";"b"],false))
let value3= E(D(["a";"b"],true),E(D(["a";"b"],true),D(["a";"b"],false)))

// Question 4
// The type of g is 'a list -> A<'a> -> 'a list, and the type of h is A<'a> -> 'a list.
// The function g takes two arguments (an empty accumulator and x) and computes a list collection which collects 
// all the elements, whose boolean value is true, in the tree.
// The function h is a wrapper of the function g, and it takes an argument x of type A<'a> and returns a list.

// Question 5
// Tail recursion occurs when a function calls itself recursively as the last thing it does before returning a value. 
// This is different from non-tail recursive functions, where the recursive call to the function happens before the final value is returned.
// In the case of the g function, it is not tail recursive because the recursive call to g happens before the final value is returned in two 
// of the cases in the match statement: C1 and C2.
// In C1, g is called with the result of another call to g as its argument. The call to g in C1 
// is not the last thing that happens before the final value is returned, so this is not tail recursion.

// In C2, g is called with a value appended to the result of the recursive call. Again, the call to g 
// in C2 is not the last thing that happens before the final value is returned, so this is also not tail recursion.

// In the third case, C3, the function simply returns the accumulator without making any recursive calls, 
// so this is tail recursion. However, since the function is not tail recursive in the other cases, it is not considered a tail recursive function overall.


// Question 6
let rec gC k acc x = 
    match x with
    | E (y,z) -> gC (fun acc -> gC k acc z) acc y
    | D (a,true) -> k (a::acc)
    | _ -> k [];;

let hC x = gC id [] x;;

/////////////
//Problem 3//
/////////////

// Question 1
let flip sq = Seq.map (fun (x,y) -> (y,x)) sq

// flip (Seq.ofList [(1,2);(3,4);(5,6)])

// Question 2
let dia n = Seq.initInfinite (fun x -> (x,n-x)) |> Seq.take (n+1)

// dia 3

// Question 3
// let allCoordinates n = 
//     let x=0
//     let rec collect x=
//         if n = 0 then Seq.empty
//         else if x=n then Seq.empty
//         else Seq.append (if x%2=0 then dia x else flip(dia x)) (collect (x+1))
//     collect x
// allCoordinates 3

let allCoordinates = Seq.concat (Seq.initInfinite (fun x -> if x%2=0 then dia x else flip(dia x)))
// allCoordinates



/////////////
//Problem 4//
/////////////

type T = Leaf of char | Branch of T*T

let t0=Branch(Leaf('a'),Branch(Leaf 'b',Leaf 'c'))
let t1=Branch(Leaf('a'),Branch(Leaf 'a',Leaf 'c'))

// Question 1
let rec toList t= 
    match t with
    | Leaf x -> [x]
    | Branch (x,y) -> toList x @ toList y

// toList t0

// Question 2
// list -> set -> list
let toSet t = List.toSeq (toList t) |> Seq.distinct |> Seq.toList

let rec legal t = (List.length (toList t)) >= 2 && List.length (toSet t) = List.length (toList t)
                      
// legal t1

// Question 3
type Dir = | L // go left
           | R // go right
type Code = Dir list
type CodingTable = Map<char, Code>

let codingTable1 :CodingTable = Map.ofList [('a',[L]);('b',[R;L]);('c',[R;R])]

let rec encode ct cl=
    match cl with
    | [] -> []
    | x::xs -> (Map.find x ct) @ encode ct xs

// Note: Map.find throws automatically an exception if the key is not found
// encode codingTable1 ['c';'a';'a';'b']

// Question 4
let rec ofT t = 
    let rec helper t acc=
        match t with
        | Leaf x -> [(x,acc)]
        | Branch (x,y) -> helper x (acc@[L]) @ helper y (acc@[R])
    Map.ofList (helper t [])
// ofT t0

// Question 5
let rec firstCharOf t c  =
    match t with
    | Leaf x -> (x,c)
    | Branch (x,y) -> match c with
                      | [] -> failwith "empty code"
                      | L::xs -> firstCharOf x xs
                      | R::xs -> firstCharOf y xs
// firstCharOf t0 [R;R;L;L;R;L]

let rec decode t c =
    if c = [] then []
    else let (x,y) = firstCharOf t c in x::decode t y

// decode t0 [R;R;L;L;R;L]