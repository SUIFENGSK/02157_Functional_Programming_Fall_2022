// Problem 1 (q 1-5)
type Multiset<'a when 'a : equality> = ('a * int) list

// q1
let getletterList xs =
    let (x,y)= List.unzip xs
    x

let duplicates x xs = (List.filter(fun x1->x1=x) (getletterList(xs))).Length >1

//duplicates "a" [("a",3);("a",5);("d",1)];

let inv xs = List.forall (fun (a,b) -> b > 0 && not(duplicates a xs)) xs

// inv [("b",3);("a",5);("d",1)]

// q2

let rec insert e n ms =
    match ms with
    | [] -> [(e,n)]
    | (a,b)::tail -> 
           if a=e then (a,b+n)::tail 
           else if (not (duplicates e ms)) then (a,b)::insert e n tail
           else insert e n tail

//insert "c" 3 [("b",3);("a",5);("d",1)]

// q3

let numberOf e ms= 
                                let option=(List.tryFind(fun (x,y)->x=e) ms)
                                if not (option=None) then 
                                    let (x,y)=option.Value
                                    y
                                else 0
//numberOf "b" [("b",3);("a",5);("d",1)]

// q4

let delete e ms = List.map (fun (x,y)-> if x=e then (x,y-1) else (x,y)) ms

// delete "b" [("b",3);("a",5);("d",1)]

// q5
let rec union (aSet,bSet) = 
        match bSet with
        | [] -> aSet
        | (x,y)::tail -> insert x y (union (aSet,tail))

//union ([("b",3);("a",5);("d",1)],[("b",3);("a",5);("d",1)])

// q6
type MultisetMap<'a when 'a : comparison> = Map<'a,int>

let invMap xs = Map.forall (fun x y-> y>0) xs
//invMap Map[("b",3);("a",5);("d",-1)]

// let insertMap e n ms = 
//                      Map.add e n ms
// insert "d" 5 [("b",3);("a",5);("d",1)]

let insertMap e k m=
     if k<=0 then failwith "insert3: argument error"
     else match Map.tryFind e m with  
          | None    -> Map.add e k m
          | Some k' -> Map.add e (k+k') m;;


// union two maps
let unionMap (map1:Map<'a,int>) (map2:Map<'a,int>)= Map.foldBack insertMap map1 map2
    // Map.fold (fun s k v ->
    //     match Map.tryFind k s with
    //     | Some v' -> Map.add k ((fun k (v, v') -> v + v') k (v, v')) s
    //     | None -> Map.add k v s) map1 map2

unionMap Map[("b",3);("a",5);("d",1)] Map[("b",3);("a",5);("d",1)]