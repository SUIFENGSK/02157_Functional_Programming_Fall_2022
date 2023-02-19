// Problem 1
type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list
let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

// q1
let rec inv sb =
     match sb with
     | [] -> true
     | (_,_,point)::_ when point<0 -> false
     | (_,_,point1)::(_,_,point2)::sbs when point1>0 && point2>0 && point1<point2 -> false
     | _::sbs -> inv sbs

// inv sb

// q2
let rec insert sc sb =
    match sb with
    | [] -> [sc]
    | x::sb -> let (_,_,point1)=x
               let (_,_,point2)=sc
               if point1<point2 then sc::x::sb
               else x::insert sc sb
// insert ("Joe", "June Fishing", 10) sb

// q3
let rec get (n,sb) =
    match sb with
    | [] -> []
    | (name,event,point)::xs -> if name = n then (event,point)::get(n,xs)
                                else get(n,xs)

// get ("Joe",sb)

// q4
let top (k:int) (sb:Scoreboard) = if k<0 || k>sb.Length then None
                                  else Some(List.take k sb)
// top 3 sb

let rec topSol k =
    function
    | _ when k = 0 -> Some []
    | [] -> None
    | s::sb -> match topSol (k-1) sb with
               | None -> None
               | Some res -> Some (s::res) 

// Problem 2
// q1
let rec replace a b ls =
   match ls with
   | [] -> []  // C1
   | x::ls -> if x=a then b :: (replace a b ls) else x::(replace a b ls) // C2

// sol:
let rec replaceSol a b =
   function
   | [] -> []
   | x::xs when x=a -> b::replace a b xs
   | x::xs -> x::replace a b xs

// replace 2 7 [1; 2; 3; 2; 4]

// q2
// replace: 'a -> 'a -> list<'a> -> list <'a>
// sol:
// a' -> b' -> c' -> d'
// a' -> b' -> c' -> d' list
// a' -> a' -> a' list -> a' list when a'=equality

// q3
// replace is not tail recursive. The function replace is called recursively and it 
// creates a huge amount of pending operations, e.g. replace 2 7 [1;2;3;2;4]
// -> 1 :: replace 2 7 [2;3;2;4]
// -> 1 :: (2 :: replace 2 7 [3;2;4])
// -> 1 :: (2 :: (3 :: replace 2 7 [2;4]))
// ......
// therefore, the declaration of f is not tail recursive.

let rec replaceA a b ls acc =
       match ls with
       | [] -> acc
       | x::ls -> if x=a then replaceA a b ls (acc@[b]) else replaceA a b ls (acc@[x])


// replaceA 2 7 [1; 2; 3; 2; 4] []

// sol

let rec replaceASol res a b =
   function
   | [] -> res
   | x::xs when x=a -> replaceASol (res@[7]) a b xs
   | x::xs -> replaceASol (res@[x]) a b xs ;;

// replaceASol [] 2 7 [1; 2; 3; 2; 4]

// Problem 3
let pos = Seq.initInfinite (fun i -> i+1) ;;
let seq1 = seq { yield (0,0); for i in pos do yield (i,i); yield (-i,-i) }
let val1 = Seq.take 5 seq1;;
let nat = Seq.initInfinite id;;
let seq2 = seq { for i in nat do yield (i,0); for j in [1 .. i] do yield (i,j) }
let val2 = Seq.toList(Seq.take 10 seq2);;

// q1
// pos -> seq<int> seq [1; 2; 3; 4; ...]
// seq1 -> seq<int*int> seq [(0, 0); (1, 1); (-1, -1); (2, 2); ...]
// val1 -> seq<int*int> first 5 value from seq1 
// seq [(0, 0); (1, 1); (-1, -1); (2, 2); (-2, -2);]

// q2
// seq2 -> seq<int*int>
// seq [(0,0);(1,0);(1,1);(2,0);(2,1);(2;2)....]
// val2 -> seq  [(0, 0); (1, 0); (1, 1); (2, 0); (2, 1); (2, 2); (3, 0); (3, 1); (3, 2); (3, 3)]

// Problem 4
type Tree<'a,'b> = | A of 'a 
                   | B of 'b
                   | Node of Tree<'a,'b> * Tree<'a,'b>;;

// q1
let value1 = Node(A true, Node(B [1;2;3], A false));;
let value2 = Node(A false, B [1;3;4])
let value3 = Node(Node(B [1;2;3], A false), Node(B [1;2;3], A false))

// q2
let rec aLeafCount t =
    match t with
    | A a -> 1
    | Node(a,b) -> aLeafCount a + aLeafCount b
    | _ -> 0

// aLeafCount value2

// q3
let rec subst a a' b b' t =
    match t with
    | A(x) when x=a -> A a'
    | A(x) -> A a
    | B(x) when x=b -> B b'
    | B(x) -> B b
    | Node(l,r) -> Node(subst a a' b b' l,subst a a' b b' r)

subst true false [1;2;3] [3;2;1] value1

// sol
let rec substSol a a' b b' t =
    match t with
    | A(x) when x=a -> A a'
    | B(x) when x=b -> B b'
    | Node(l,r) -> Node(subst a a' b b' l,subst a a' b b' r)
    | leaf -> leaf

// q4
let rec g = function
    | Node(t1,t2) -> Node(g t2, g t1)
    | leaf -> leaf;;

let rec f = function
    | A a -> ([a],[])
    | B b -> ([], [b])
    | Node(t1,t2) -> let (xs1,ys1) = f t1
                     let (xs2,ys2) = f t2
                     (xs1@xs2, ys1@ys2);;

// g: Tree<'a,'b> -> Tree<'a,'b>, flip the tree
// f: Tree<'a,'b> -> 'a list * 'b list, print the tree with type
f value1

// q5
let rec fk k = function
    | A a -> k ([a],[])
    | B b -> k ([], [b])
    | Node(t1,t2) -> let (xs1,ys1) = fk k t1
                     let (xs2,ys2) = fk k t2
                     k (xs1@xs2, ys1@ys2);;
fk id value1

// sol
let rec fC c = function
    | A a -> c ([a],[])
    | B b -> c ([], [b])
    | Node(t1,t2) -> fC (fun (xs1,ys1) -> fC (fun (xs2,ys2) -> c (xs1@xs2, ys1@ys2)) t2) t1;;


// Problem 5
type T<'a> = N of 'a * T<'a> list;;
let td = N("g", []);;
let tc = N("c", [N("d",[]); N("e",[td])]);;
let tb = N("b", [N("c",[])]);;
let ta = N("a", [tb; tc; N("f",[])])

// q1
let rec toList (t: T<'a>) =
    match t with
    | N(v,[]) -> [v]
    | N(v,ts) -> v :: List.concat (List.map toList ts)

// toList ta

let rec toListSol (N(v,ts)) = v :: toListAux ts
and toListAux = 
    function
    | [] -> []
    | t::ts -> toListSol t @ toListAux ts

// q2
let rec map f t =
    match t with
    | N(v,[]) -> N(f v, [])
    | N(v, ts) -> N(f v, List.map (fun x -> map f x) ts)

let rec mapSol f (N(v,ts)) = N(f v, List.map (map f) ts)

// q3
type Path = int list;;

let rec isPath path t =
    match (path,t) with
    | ([], _) -> true
    | (i::path', N(v,ts)) when 0<=i && i<List.length ts -> isPath path' (List.item i ts)
    | _ -> false

// q4
let rec get1 path (N(_,ts) as t) =
    match path with
    | [] -> t
    | i::is -> get1 is (List.item i ts)

// q5
let rec tryFindPathTo v (N(v',ts)) =
    if v=v' then Some [] else tryFindInList 0 v ts
and tryFindInList i v = function
   | [] -> None
   | N(v',_) :: _ when v = v' -> Some[i]
   | N(_,ts') :: ts -> match tryFindInList 0 v ts' with
                       | None -> tryFindInList (i+1) v ts
                       | Some is -> Some (i::is)
