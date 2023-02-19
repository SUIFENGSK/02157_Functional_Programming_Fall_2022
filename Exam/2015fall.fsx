// Problem 1
type Appliance = string
type Usage = Appliance * int
let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2]

// q1
let rec inv ats =
    match ats with
    | []-> true
    | (a,t)::xs -> t>0 && inv xs

// inv ats

// q2
let rec durationOf a ats =
    match ats with
    | [] -> 0
    | (a1,t1)::xs -> if a1=a then t1 + durationOf a xs
                     else durationOf a xs

// durationOf "washing machine" ats

// q3
let isWF ats = List.forall (fun(a,t) -> durationOf a ats <= 24) ats && inv ats

// isWF ats

// q4
let rec delete (a,ats) = 
    match ats with
    | [] -> []
    | (a1,t1)::xs -> if a1=fst(a) then delete(a,xs)
                     else (a1,t1)::delete(a,xs)

// delete (ad2,ats)

// q5
type Price = int
type Tariff = Map<Appliance, Price>

let isDefined ats trf = List.forall (fun(a,t) -> Map.containsKey a trf) ats
let trf = Map.ofList [("washing machine", 2); ("coffee machine", 2); ("dishwasher", 2)]
// isDefined ats trf

// q6
let rec priceOf ats trf =
    match ats with
    | [] -> 0
    | (a,t)::xs -> if Map.containsKey a trf then t*(Map.find a trf) + priceOf xs trf
                   else failwith "Not defined"

// priceOf ats trf

// Problem 2
let rec g1 p = function
    | x::xs when p x -> x :: g1 p xs
    | _ -> [];;

let rec g2 f h n x =
    match n with
    | _ when n<0 -> failwith "negative n is not allowed"
    | 0 -> x
    | n -> g2 h f (n-1) (f x);;

// q1
// g1 computes a list. Essentially computes a list as long as it satisfies a predicate.
g1 (fun x -> x % 2 = 0) [2;3;4;5;6;7;8;9;10]

g2 (fun x -> x + 1) (fun x -> x * 2) 3 1

// q2
let rec g1A p a = function
    | x::xs when p x -> g1A p (x::a) xs 
    | _ -> List.rev a

g1A (fun x -> x > 0) [] [2;3;4;5;6;7;8;9;10]


let rec g1C p c = function
    | x::xs when p x -> g1C p (fun v -> c (x::v)) xs 
    | _ -> c []

g1C (fun x -> x > 0) id [2;3;4;5;6;7;8;9;10]

// q3
// g2 is tail recursive as it calculates things first then call it self, avoiding pending calculations. 
// As an example, to call g2 again, the new n: (n-1), the return of (f x) will be calculated first then 
// be used in the recursive call, doing the work up front.
// This consequently means the current stack frame is no longer needed. ie. return (return (return (return 5)))) == return 5. 
// There is no longer any pending calculations needed. 

// q4
let f1 m n k = seq { for x in [0..m] do
                                for y in [0..n] do
                                if x+y < k then yield (x,y) }
                                
let f2 f p sq = seq { for x in sq do
                                if p x then yield f x }

let f3 g sq = seq { for s in sq do yield! g s }

let value = List.ofSeq(f1 2 2 3)
// [(0, 0); (0, 1); (0, 2); (1, 0); (1, 1); (2, 0)]

// q5
let f2Alt f p sq = Seq.init (Seq.length sq) (fun i -> f (Seq.item i sq))
// f2 (fun x -> x + 1) (fun x -> x > 0) [2;3;4;5;6;7;8;9;10]
// f2Alt (fun x -> x + 1) (fun x -> x > 0) [2;3;4;5;6;7;8;9;10]

// q6
// f1 computes a list. Essentially computes a list as long as it satisfies a predicate.(x+y < k)
// f1 int -> int -> int -> seq<int * int>
// f2 computes a list. Essentially computes a list as long as it satisfies a predicate.(if p x then yield f x)
// f2 ('a -> 'b) -> ('a -> bool) -> seq<'a> -> seq<'b>
// f3 computes a list. Essentially computes a list as long as it satisfies a predicate.(yield! g s)
// f3 ('a -> seq<'c>) -> sq: seq<'a> -> seq<'c>

// Problem 3
type Name = string
type Flow = int // can be assumed positive in below questions
type River = R of Name * Flow * Tributaries
            and Tributaries = River list

// q1
let riv1=R("R1",5,[])
let riv4=R("R4",2,[])
let riv2=R("R2",15,[riv4])
let riv3=R("R3",8,[])
let riv=R("R",10,[riv1;riv2;riv3])

// q2
let getNameAndTributaries r =
    match r with
    | R(name,_,tri) -> (name,tri)

let rec contains (n:Name) (r:River) : bool =
    let name = fst (getNameAndTributaries r)
    let tributaries = snd (getNameAndTributaries r)
    if name = n then true
    else
        let rec test tributaries =
            match tributaries with
            | [] -> false
            | h::t -> contains n h || test t
        test tributaries

// contains "R4" riv

// q3
let rec allNames (r:River) : list<Name> =
    match r with
    | R(name,_,tri) -> name::(List.concat (List.map allNames tri))

allNames riv

// q4
let rec getAllFlow (r:River) : list<Flow>=
        match r with
        | R(_,flow,tri) -> flow::(List.concat (List.map getAllFlow tri))

let totalFlow (r:River) : Flow =
    let rec calcAllFlow flows =
         match flows with
         | [] -> 0
         | x::xs -> x+calcAllFlow xs
    calcAllFlow (getAllFlow r)

// totalFlow riv

// q5
let mainSource (r:River) =
   let names = allNames r
   let flows = getAllFlow r
   let rec findMaxFlow flows names =
         match flows with
         | [] -> failwith "no flow"
         | x::xs -> match names with
                    | [] -> failwith "no name"
                    | y::ys -> if x = List.max flows then (y,x)
                               else findMaxFlow xs ys
   findMaxFlow flows names

// mainSource riv3

// q6
let rec findRiver n r = function 
    | [] -> []
    | (R (n',f',t))::rs when n = n' -> (R (n',f',(r::t)))::rs
    | r'::rs -> r'::(findRiver n r rs);;


let tryInsert n t = function 
    | (R (n',f',t')) when (n = n') ->  Some (R (n',f',t::t'))
    | (R (n',f',t')) when (contains n (R (n',f',t'))) -> 
        let newt = findRiver n t t'
        Some (R (n',f',newt))
    | (R (n',f',t')) -> None;;

tryInsert "R" riv4 riv