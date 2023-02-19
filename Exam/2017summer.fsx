// Problem 1
// q1
let rec repeatList xs n = 
    match n with
    | 0 -> []
    | _ -> xs @ (repeatList xs (n-1))
// repeatList [1;2] 3

// q2
let rec merge (m,n) = 
    match (m,n) with
    | ([],_) -> n
    | (_,[]) -> m
    | (x::xs,y::ys) -> x::y::merge(xs,ys)

// merge ([1;2;3;4],[5;6])

// Problem 2
let rec f = function
           | 0 ->[0]
           | i when i>0 -> i::g(i-1)
           | _ -> failwith "Negative argument"
and g = function
        | 0 -> []
        | n -> f(n-1)

let h s k = seq {for a in s do yield k a}

let rec sum xs = match xs with
                 | [] -> 0
                 | x::rest -> x+ sum rest
// q1
f 5
h (seq [1;2;3;4]) (fun i -> i+10)

// q2
let rec sumA xs acc =
    match xs with
    | [] -> acc
    | x::rest -> sumA rest x+acc

// sum [1;2;3;4]
// sumA [1;2;3;4] 0

let rec sumC xs k =
    match xs with
    | [] -> k 0
    | x::rest -> sumC rest (fun v->k(x+v)) 

// sumC [1;2;3;4] id

// Problem 3
type Article = string
type Amount = int
type Price = int
type Desc = Amount*Price
type Stock = (Article*Desc) list
let st = [("a1",(100,10)); ("a2",(50,20)); ("a3",(25,40))];;

// q1
let rec value st =
    match st with
    | [] -> 0
    | (_,(n,p))::sts -> n*p+value sts
// value st

// q2
let rec getAllArticle st =
    match st with
    | [] -> []
    | (a,(_,_))::xs -> a::getAllArticle xs

let rec checkNeg st =
    match st with
    | [] -> true
    | (a1,(n1,p1))::xs -> n1>0 && p1>0 && checkNeg xs

let inv st = List.length st = List.length (List.distinct (getAllArticle st)) && checkNeg st

// inv st
type Order = Article*Amount
type Status<'a> = Result of 'a | Error of string

// q3
let rec getHelper(a,k) st
    = match st with
      | [] -> []
      | (a1,(n1,p1))::xs -> if a=a1 then (a1,(n1-k,p1))::getHelper(a,k) xs
                            else (a1,(n1,p1))::getHelper(a,k) xs
let get(a,k) st=
    let original = st
    let rec getrec(a,k) st
        = match st with
          | [] -> Error "Not found"
          | (a1,(n1,p1))::xs -> if a=a1 && k<=n1 then Result(k*p1,getHelper (a,k) original)
                                else if a=a1 && k>n1 then Error ("Insufficient supply for "+a)
                                else getrec(a,k) xs
    getrec(a,k) st

// get ("a2",60) st

// q4
let rec getAll os st = match os with
                       | [] -> Result (0, st)
                       | (a,k)::xs -> match get(a,k) st with
                                        | Error s -> Error s
                                        | Result (v,st1) -> match getAll xs st1 with
                                                             | Error s -> Error s
                                                             | Result (v1,st2) -> Result (v+v1,st2)
getAll [("a1",10);("a2",10);("a3",10)] st

// Problem 4
type T<'a> = | L
             | A of 'a * T<'a>
             | B of 'a * T<'a> * T<'a>
             | C of 'a * T<'a> * T<'a> * T<'a>

// q1
// let value1 = C(1,B(1,L,L),A(1,L),L)

// q2
let rec f1 t = 
  match t with
    | B(_, t1,t2) -> f1 t1 && f1 t2
    | L -> true
    | _ -> false

let rec f2 t = 
  match t with
    | L -> L
    | A(i,t) -> A(i, f2 t)
    | B(i,t1,t2) -> B(i, f2 t2, f2 t1)
    | C(i,t1,t2,t3) -> C(i, f2 t3, f2 t2, f2 t1)

let rec f3 h = function
    | L -> L
    | A(i,t) -> A(h i, f3 h t)
    | B(i,t1,t2) -> B(h i, f3 h t1, f3 h t2)
    | C(i,t1,t2,t3) -> C(h i, f3 h t1, f3 h t2, f3 h t3)

// val f1: t: T<'a> -> bool
// val f2: t: T<'a> -> T<'a>
// val f3: h: ('a -> 'b) -> _arg1: T<'a> -> T<'b>

// Problem 5
type Title = string
type Document = Title * Element list
and Element = Par of string | Sec of Document;;
let s1 = ("Background", [Par "Bla"])
let s21 = ("Expressions", [Sec("Arithmetical Expressions", [Par "Bla"]);
Sec("Boolean Expressions", [Par "Bla"])])
let s222 = ("Switch statements", [Par "Bla"])
let s223 = ("Repeat statements", [Par "Bla"])
let s22 = ("Statements",[Sec("Basics", [Par "Bla"]) ; Sec s222; Sec s223])
let s23 = ("Programs", [Par "Bla"])
let s2 = ("The Programming Language", [Sec s21; Sec s22; Sec s23])
let s3 = ("Tasks", [Sec("Frontend", [Par "Bla"]);
Sec("Backend", [Par "Bla"])])
let doc = ("Compiler project", [Par "Bla";Sec s1;Sec s2;Sec s3]);;

// q1
let rec noOfSecs d =
    match d with
    | (t,[]) -> 0
    | (t,e::es) -> noOfSecshelper e + noOfSecs (t,es)
and noOfSecshelper e =
    match e with
    | Par s -> 0
    | Sec d -> 1 + noOfSecs d

// noOfSecs doc

// q2
let rec sizeOfDoc d =
   match d with
   | (t,[]) -> String.length t
   | (t,e::es) -> sizeOfDochelper e + sizeOfDoc (t,es)
and sizeOfDochelper e =
    match e with
    | Par s -> String.length s
    | Sec (t,el) -> sizeOfDoc (t,el)

// sizeOfDoc doc


// q3
let titlesInDoc d  =
    let rec titlesInDocHelper d =
        let (title, elements) = d
        let titles = elements |> List.map (function
            | Sec s -> titlesInDocHelper s
            | _ -> []
        )
        title::List.concat titles
    List.removeAt 0 (titlesInDocHelper d)
    

// titlesInDoc doc

type prefix = int list
type ToC = (prefix*Title) list

// q4
let rec toc d prefix =
    let (title, elements) = d
    let toc' i = function
        | Sec s -> toc s (prefix @ [i])
        | _ -> []
    let tocs = List.mapi toc' elements
    (prefix, title) :: List.concat tocs

toc doc []