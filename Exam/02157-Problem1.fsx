(* Solution to Problem 1 from exam winter 2013 in 02157        Michael R. Hansen 
                                                                      07-10-2015
                                                                      06-10-2021  *)  

type Multiset<'a when 'a : equality> = ('a * int) list;;

(* Q1 *)

// An approach using a recursive helper function
// Traverse the multiset an check for each (e,k) 
// - e is not previously met (contained in es)
// - k is positive
let rec inv0h es = function
                   | [] -> true
                   | (e,k)::m -> k>0 && not(List.contains e es) && inv0h (e::es) m;;

let inv0 m = inv0h [] m;;               


// A solution mixing recursion and use of List.forall
let rec inv1 = function
               | [] -> true
               | (e,k)::m -> k>0 && List.forall (fun (e',_) -> e <> e') m && inv1 m;;

// A solution similar to inv0 buit using sets
let rec inv2h es = function 
                   | []                                       -> true
                   | (e,k)::_ when k<=0 || Set.contains e es  -> false
                   | (e,_)::ms                                -> inv2h (Set.add e es) ms;;

let inv2 m = inv2h Set.empty m;;                   
      
let ms1 = [("b",3); ("a",5); ("d",1)];;


(* Q2 *)
let rec insert e k m = 
   if k <= 0 then failwith "multiset: argument error"
   else match m with 
        | []                    -> [(e,k)]
        | (e',k')::m' when e=e' -> (e,k+k')::m'
        | (e',k')::m'           -> (e',k')::insert e k m';;


(* Q3 *)
let rec numberOf e = function
                     | []                  -> 0
                     | (e',k)::m when e=e' -> k
                     | _::m'               -> numberOf e m';;


// A solution using List.tryFind

let numberOf1 e m = match List.tryFind (fun (e',_) -> e=e') m with
                    | None      -> 0
                    | Some(_,n) -> n

(* Q4 *)
let rec delete e = function
                   | [] -> []
                   | (e',1)::m when e=e' -> m
                   | (e',k)::m when e=e' -> (e,k-1)::m
                   | (e',k)::m           -> (e',k)::delete e m;;


(* Q5 *)

let rec union(m, m2) = 
   match m with
   | []        -> m2
   | (e,k)::m1 -> union(m1, insert e k m2);;

let union1(m1, m2) = List.foldBack (fun (e,k) m -> insert e k m)  m1 m2;;
 
let mu =  union ([("b",3); ("a",5); ("d",1)], [("a",3); ("b",4); ("c",2)]);; 

(* Q6 *)

type MultisetMap<'a when 'a : comparison> = Map<'a,int>;;

let inv3 m = Map.forall (fun _ n -> n>0) m;;    

let insert3 e k m = 
    if k<=0 then failwith "insert3: argument error"
    else match Map.tryFind e m with  
         | None    -> Map.add e k m
         | Some k' -> Map.add e (k+k') m;;

let union2(m1,m2) = Map.foldBack insert3 m1 m2;;

let mu1 =  union2(Map.ofList [("b",3); ("a",5); ("d",1)], Map.ofList [("a",3); ("b",4); ("c",2)]);;                                                                   