// 02157 Functional programming - Assignment 1
// Name: Shuokai Ma
// Student number: s214919
// Date: 30/10/2022

/////////////
//Problem 1//
/////////////
type Book = string
type Shelf = Book list // ordered alphabetically
type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 = ["Introduction to meta-mathematics";"To mock a mockingbird";"What is the name of this book"]
let ls0 = [("Communication and concurrency", "Bob", 4);("Programming in Haskell", "Paul", 2);("Communicating Sequential processes", "Mary", 7);("Elements of the theory of computation", "Dick", 1)]

// Question 1
let rec onShelf book shelf =
       match shelf with
       | [] -> false
       | b::bs -> b=book || onShelf book bs

(*Feedback: Good*)

// onShelf "b" ["a";"b";"c"]

// Question 2
let rec toShelf book shelf=
        match shelf with
        | [] -> [book]
        | b::bs -> if b=book then shelf else if b>book then book::shelf else b::toShelf book bs
    
(*Feedback: Remember, the shelf is ordered in alphabetical order and should remain ordered
after insertion of a new book*)

// toShelf "c" ["a";"f";"g"]

// Question 3
let rec fromShelf book shelf=
        match shelf with
        | [] -> None
        | b::bs -> if b=book then Some bs 
                                 else match fromShelf book bs with
                                       | None -> None
                                       | Some bs -> Some (b::bs)

(*Feedback: Good*)

// fromShelf "b" ["a";"b";"c"]


// Question 4
let addLoan b n d ls = ls @ [(b,n,d)]

(*Feedback: Good*)

// addLoan "a" "123" 1 [("Communication and concurrency", "Bob", 4)]

let rec removeLoan b n ls = 
        match ls with
        | [] -> []
        | (bx,nx,dx)::lss -> if bx=b && nx=n then lss else (bx,nx,dx)::removeLoan b n lss

(*Feedback: Good*)

// removeLoan "Programming in Haskell" "Paul" ls0

// Question 5
let rec reminders d0 ls =
       match ls with
       | [] -> []
       | (b,n,d)::lss -> if d<d0 then (n,b)::reminders d0 lss else reminders d0 lss

(*Feedback: Good*)

// reminders 3 ls0

// Question 6
let rec toLetters nb=
        match nb with
        | [] -> []
        | (n,b)::xs -> let text="Dear " + n + "!\n" + "Please return " + "\""+b+"\".\n"+"Regards Robin" in text::toLetters xs

(*Feedback: Good*)

// toLetters [("Bob","Communication and concurrency");("Paul","Programming in Haskell")]

// Question 7.1
let toLettersMap nb = List.map (fun (n,b) -> "Dear " + n + "!\n" + "Please return " + "\""+b+"\".\n"+"Regards Robin") nb

(*Feedback: Good*)

// toLettersMap [("Bob","Communication and concurrency");("Paul","Programming in Haskell")]

// Question 7.2
let remindersFoldBack d0 ls= List.foldBack (fun (b,n,d) acc -> if d<d0 then (n,b)::acc else acc) ls []

(*Feedback: Good*)

// remindersFoldBack 3 ls0

/////////////
//Problem 2//
/////////////

// Question 1
let rec f x = function
    | [] -> []  // (C1)
    | y::ys -> (x,y)::f x ys // (C2)
// The type of f must have the form 'a -> 'b list -> ('a * 'b) list
// due to the form ... f x = function | []->[] ... where
// 'a and 'b are fresh type varibales and 
// x: 'a
// the patterns [] and y::ys in (C1) and (C2) must have same type as 'b, that is, 
// y: 'b and ys: 'b list in (C2)
// the experssions [] and (x,y)::f x ys in (C1) and (C2) must have same type as ('a * 'b) list, that is,
// (x,y): ('a * 'b) and f x ys : ('a * 'b) list (due to "::")
// Since there are no further constarints, f: 'a -> 'b list -> ('a * 'b) list is the most general type

let rec allPairs xs ys =
    match xs with
    | [] -> []
    | x::xrest -> f x ys @ allPairs xrest ys
// The type of allPairs must have the form 'a list -> 'b list -> ('a * 'b) list
// due to the form ... allPairs xs ys = match xs with | []->[] ... where
// 'a and 'b are fresh type varibales and 
// xs: 'a list
// the patterns [] and x::xrest in (C1) and (C2) must have same type as 'a list due to "match xs with", that is, 
// x: 'a and xs: 'a list in (C2)
// the experssions [] and f x ys @ allPairs xrest ys in (C1) and (C2) must have same type as ('a * 'b) list due to f type and "@", that is,
// f x ys: ('a * 'b) list and allPairs xrest ys : ('a * 'b) list
// Since there are no further constarints, allParis: 'a list -> 'b list -> ('a * 'b) list is the most general type

(*Feedback: Correct*)

// Question 2
// f "a" [1;2;3]
// => ("a",1)::f "a" [2;3]
// => ("a",1)::(("a",2)::f "a" [3])
// => ("a",1)::(("a",2)::(("a",3)::f "a" []))
// => ("a",1)::(("a",2)::(("a",3)::[]))
// => ("a",1)::(("a",2)::[("a",3)])
// => ("a",1)::[("a",2);("a",3)]
// => [("a", 1); ("a", 2); ("a", 3)]

(*Feedback: Good*)

// Question 3
// Consider the expression f "a" [1;2;3] and notice
// "a": string and [1;2;3]: int list
// Substituting string for 'a and int for 'b in the most general type
// f: 'a -> 'b list -> ('a * 'b) list
// gives the instantiation
// string -> int list -> (string * int) list
// That is the type of f in f "a" [1;2;3]

(*Feedback: Good*)

// Question 4
let fh s l=List.map (fun x->(s,x)) l
// fh "a" [1;2;3];;
(*Feedback: Good*)

/////////////
//Problem 3//
/////////////

(*
type T = One of int | Two of int * T * int * T
let rec f p t =
    match t with
    | One v when p v -> [v] (* C1 *)
    | Two(v1,t1,_,_) when p v1 -> v1::f p t1 (* C2 *)
    | Two(_,_,v2,t2) -> v2::f p t2 (* C3 *)
    | _ -> [];; (* C4 *)
*)

// Question 1
// The type of f should have the form (int -> bool) -> T -> int list
// f takes in two arguments (p and t) and computes a list of integers from a tree t of type T which satisfy a predicate p of type int -> bool
(*Feedback: Type is correct, but add some more explanation about how the function chooses with brach to take (C2 and C3)*)

// Question 2

(*
let p x = x > 0
let t1 = One 1   (* C1 *)
let t2 = Two(5, t1, 3, t1) (* C2 *)
let t3 = Two(-5, t1, 4, t1) (* C3 *)
let t4 = One 0 (* C4 *)
*)

(*Feedback: Good*)

/////////////
//Problem 4//
/////////////

// Part 1: A simple stack machine
type Instruction = | ADD | SUB | SIGN | ABS | PUSH of int
type Stack = int list
let intpInstr instruction stack =
    match (instruction,stack) with
    | (ADD,a::b::st) -> (b+a)::st
    | (SUB,a::b::st) -> (b-a)::st
    | (SIGN,a::st) -> -a::st
    | (ABS,a::st) -> abs(a)::st
    | (PUSH r,_) -> r::stack  
    | _ -> stack

(*Feedback: Good*)

// intpInstr ADD [1;2;3]

let exec instructions= 
        let result = List.fold (fun st instr -> intpInstr instr st) [] instructions
        match result with
        | top::st -> top
        | _ -> failwith ("The stack is empty")
// exec [PUSH 1; PUSH 2; PUSH 3; ADD]

(*Feedback: Good, nice error handling.
But note that the function could also fail, for instance, if you call ADD on a stack with only one element*)

// Part 2: Expressions: Syntax and semantics
type Exp = X | C of int | Minus of Exp | Abs of Exp | Add of Exp*Exp | Sub of Exp*Exp 
let rec sem e x =
    match e with
    | X -> x
    | C c -> c
    | Minus e -> - (sem e x)
    | Abs e -> abs(sem e x)
    | Add (e1,e2) -> sem e1 x + sem e2 x
    | Sub (e1,e2) -> sem e1 x - sem e2 x

(*Feedback: Good*)

// sem (Sub (C 1, C 2)) 0

// Part 3: Compilation to stack-machine code
let rec compile e x =
    match e with
    | X -> [PUSH x]
    | C c -> [PUSH c]
    | Minus e -> (compile e x) @ [SIGN]
    | Abs e -> (compile e x) @ [ABS]
    | Add (e1,e2) -> (compile e1 x) @ (compile e2 x) @ [ADD]
    | Sub (e1,e2) -> (compile e1 x) @ (compile e2 x) @ [SUB]

(*Feedback: Good*)

// compile (Sub (C 1, C 2)) 0

#r "nuget: FsCheck"
let check e x = exec (compile e x) = sem e x
let t = FsCheck.Check.Quick check

(*Feedback: Good assignment overall*)