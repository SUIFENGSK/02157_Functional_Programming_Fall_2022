// Problem 1
type Prize = String
type Achievement = int
type PrizeTable = (Prize * Achievement) list

// q1
let rec inv pt=
    match pt with
    | (p1,a1)::(p2,a2)::xs -> if a1<a2 then inv ((p2,a2)::xs) else false
    | _ -> true

let pt =[("p1",3);("p2",5);("p3",8);("p4",11)]
// inv pt

// q2
let rec prizesFor a pt =
    match pt with
    | [] -> []
    | (p1,a1)::pt -> if a1<=a then p1::(prizesFor a pt) else prizesFor a pt

// prizesFor 7 pt

// q3
let rec increase k pt =
    match pt with
    | [] -> []
    | (p1,a1)::pt -> (p1,a1+k)::(increase k pt)

// increase 2 pt

// q4
let rec add ((p,a),pt) =
    match pt with
    | [] -> []
    | (p',a')::pt -> if a'<a then (p',a')::(add ((p,a),pt))
                     elif a'>a then (p,a)::(p',a')::pt
                     else failwith "exist"

// add (("p35",10),pt)

// q5
let rec merge pt1 pt2 =
    match pt1 with
    | [] -> pt2
    | (p1,a1)::pt1 -> merge pt1 (add ((p1,a1),pt2))

// let rec merge pt1 pt2 =
//     match (pt1,pt2) with
//     | ([],_) -> pt2
//     | (_,[]) -> pt1
//     | ((p1,a1)::pt1,(p2,a2)::pt2) -> if a1<a2 then (p1,a1)::(merge pt1 ((p2,a2)::pt2))
//                                      elif a1>a2 then (p2,a2)::(merge ((p1,a1)::pt1) pt2)
//                                      else failwith "error"

let pt1 =[("p1",3);("p2",5);("p3",8);("p4",11)]
let pt2 =[("p5",4);("p6",6);("p7",7);("p8",9)]
merge pt1 pt2

// q6
let prizesForL a pt = 
           let newList = List.filter (fun (p1,a1)-> a1<a) pt
           List.map (fun(p1,a1)->p1) newList
// prizesForL 7 pt

let increaseL k pt = List.map (fun(p1,a1)->(p1,a1+k)) pt
// increaseL 2 pt

let mergeL pt1 pt2 = List.foldBack (fun (p1,a1) pt2 -> add ((p1,a1),pt2)) pt1 pt2
mergeL pt1 pt2

// Problem 2
let rec choose f xs =
    match xs with
    | [] -> [] // C1
    | x::rest -> match f x with
                 | None -> choose f rest // C2
                 | Some y -> y::(choose f rest)  //C3
// q1
// Let ’a and ’b be fresh type variables so that f:'a and xs:'b.
// Since xs is matched with pattern [] in (1), xs must have type ’c list ( 'c fresh) and 'b = 'c list
// The value of the function must have type 'd list ( 'd fresh) due to the expression [] in (1)
// Since xs:'c list is matched with pattern x::rest in (2), we have x:'c and rest:'c list due to the type of cons ::.
// Since the value of the function must have type 'd list, y::(choose f rest) in (2) must have type 'd list.
// y must have type 'd due to the type of cons ::.
// Since f x is matched with pattern Some y in (3), we have f 'c-> option 'd. And we know 'b='c, so we have f 'b-> option 'd.
// And then we have 'a = 'b-> option 'd.
// So we have ('b-> option 'd) -> 'b list -> 'd list
// We can simplify it to ('a-> option 'b) -> 'a list -> 'b list

// q2
let chEven n = if n%2=0 then Some n else None
// -> choose chEven [1;2;3;4;5]
// -> chEven 1
// -> choose chEven [2;3;4;5]
// -> chEven 2
// -> 2::(choose chEven [3;4;5])
// -> 2::(chEven 3)
// -> 2::(choose chEven [4;5])
// -> 2::(chEven 4)
// -> 2::(4::(choose chEven [5]))
// -> 2::(4::(chEven 5))
// -> 2::(4::(choose chEven []))
// -> 2::(4::[])
// -> 2::[4]
// -> [2;4]

// q3
let rec chooseA f xs acc =
    match xs with
    | [] -> acc // C1
    | x::rest -> match f x with
                 | None -> chooseA f rest acc // C2
                 | Some y -> chooseA f rest (acc@[y])  //C3

chooseA chEven [1;2;3;4;5] []

let rec chooseC f xs k =
    match xs with
    | [] -> k [] // C1
    | x::rest -> match f x with
                 | None -> chooseC f rest k // C2
                 | Some y -> chooseC f rest (fun ys -> k (y::ys))  //C3
chooseC chEven [1;2;3;4;5] id

// Problem 3
type T = | One of int | Two of int * T * int * T
let rec f p t =
   match t with
    | One v when p v -> [v]
    | Two(v1,t1,_,_) when p v1 -> v1 :: f p t1
    | Two(_,_,v2,t2) -> v2 :: f p t2
    | _ -> []

// q1
// val f: p: (T -> bool) -> t: T -> T list
// The function f takes a function p and a tree t, and returns a list of values in left children of every node in the t 
// that satisfy p else returns all values in right children of every node.

// q2
let p = fun x -> x>3
let t = Two(1,One 2,3,Two(4,One 5,6,One 7))
// f p t

// Problem 4
type Trie<'a> = N of 'a * bool * Children<'a>
and Children<'a> = Trie<'a> list

let t1 = N(0, false, [N(0,false, [N(1,true, [])])])
let t2 = N(0, true, [N(0,false, [N(1,true, [])])])
let ta = N(1, true, [N(2,true,[])])
let tb = N(3, false, [N(0,true,[])])
let tc = N(2,true,[])
let t3 = N(0, false, [ta;tb;tc])

// q1
let rec count t =
   match t with
   | N(_,_,ns) -> if ns=[] then 1
                  else 1+countChildren ns
and countChildren ns =
   match ns with
   | [] -> 0
   | n::ns -> count n + countChildren ns

// count t3

// q2
let rec accept w (N (x, y, ls): Trie<'a>) =
    match w with
    | [] -> true
    | [ w1 ] when x = w1 -> true
    | head :: tail when x = head -> List.exists (accept tail) ls
    | _ -> false

// accept [0;1;2] t3

// q3
let rec wordsOf' (N (a, b, ch): Trie<'a>) =
    match b with
    | true -> [ [ a ] ] @ (List.map (fun x -> a :: x) (List.collect wordsOf' ch))
    | false -> List.map (fun x -> a :: x) (List.collect wordsOf' ch)

let rec wordsOf t = Set.ofList (wordsOf' t)

wordsOf t3

// q4
let rec uselessLeaves (N (a, b, ch): Trie<'a>) =
    match ch with
    | [] when b -> false
    | [] -> true
    | ch -> List.exists uselessLeaves ch

//q5
let rec degree (N (a, b, ch): Trie<'a>) =
    List.max (List.length ch :: (List.map degree ch))