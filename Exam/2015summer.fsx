// Problem 1
// q1
let rec repeat s n =
    match s with
    | "" -> ""
    | _ when n = 0 -> ""
    | s -> s + repeat s (n-1)

// repeat "ab" 0

// q2

// let f s1 s2 n =
//     let mutable result = ""
//     for i in 1..n do
//         result <- result + (if i % 2 = 0 then s2 else s1) + "\n"
//     result

let f s1 s2 n =
    let rec build result i =
        if i > n then result
        elif i=n then build (result + (if i % 2 = 0 then s2 else s1)) (i + 1)
        else build (result + (if i % 2 = 0 then s2 else s1) + "\n") (i + 1)
    build "" 1

// val f: s1: string -> s2: string -> n: int -> string    
// f "ab" "cd" 4
// f "XO" "OX" 3

// q3
let viz m n = let s1 = repeat "xo" m
              let s2 = repeat "ox" m
              f s1 s2 n

// viz 4 5

// q4
let rec repeatA s n acc =
    match s with
    | "" -> acc
    | _ when n=0 -> acc
    | s -> repeatA s (n-1) (s+acc)

// repeatA "ab" 3 ""

let rec repeatC s n k =
    match s with
    | "" -> k ""
    | _ when n = 0 -> k ""
    | s -> repeatC s (n-1) (fun v -> k(s+v))

// repeatC "ab" 3 id



// Problem 2
let mixMap f xs ys = List.map f (List.zip xs ys)
let unmixMap f g xs = 
                                  let (x,y) =List.unzip xs
                                  (List.map f x, List.map g y)
// type
// mixMap f: ('a * 'b -> 'c) -> xs: 'a list -> ys: 'b list -> 'c list
// unmixMap f: ('a -> 'b) -> g: ('c -> 'd) -> xs: ('a * 'c) list -> 'b list * 'd list


// Problem 3
type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>;;
let t = Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf)))

// q1
let rec reflect t =
    match t with
    | Lf -> Lf
    | Br(a,n,b) -> Br(reflect b, n, reflect a)

// reflect t

// q2

let rec accumulate tree =
    let rec aux acc = function
        | Lf -> Lf
        | Br(left, value, right) -> Br(aux (acc + value) left, acc + value, aux (acc + value) right)
    aux 0 tree

// This implementation of accumulate uses an auxiliary function aux to traverse the tree in a pre-order fashion,
// keeping track of the accumulated value at each node in the acc parameter. The aux function takes the current 
// accumulated value and the current tree node as its arguments, and returns the corresponding accumulated tree 
// node. When the aux function encounters a leaf node (Lf), it simply returns Lf. Otherwise, it recursively calls
// itself on the left and right subtrees of the current node, passing the updated accumulated value as an argument. 
// The accumulate function itself simply calls the aux function with an initial accumulated value of 0.


// accumulate t

// q3
let rec k i t =
    match t with
    | Lf -> Lf
    | Br(tl,a,tr) -> Br(k (i*i) tl, i*a, k (i*i) tr)

let rec h n m =
    function
    | Br(tl,a,tr) when n=m -> h n 1 tl @ [a] @ h n 1 tr
    | Br(tl,_,tr) -> h n (m+1) tl @ h n (m+1) tr
    | Lf -> []

let q n t = h n n t

// val k: i: int -> t: Tree<int> -> Tree<int>
// val h: n: int -> m: int -> _arg1: Tree<'a> -> 'a list
// val q: n: int -> t: Tree<'a> -> 'a list  

// k computes the k-th power of each element in the tree, depending on its depth in the tree.
// q computes the elements at depth n in the tree. The function h is used to compute the elements at depth n in the tree.





// Problem 4
type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>

// q1
let isValidCourseDesc course = 
                                      let (t,e)=course
                                      if e%5=0 then true else false
// let c:CourseDesc = ("123",4);
// isValidCourseDesc c

// q2
let isValidCourseBase courseBase = Map.forall (fun x course -> isValidCourseDesc course) courseBase
// isValidCourseBase Map[1,("123",10);2,("123",5)]

type Mandatory = Set<CourseNo>
type Optional = Set<CourseNo>
type CourseGroup = Mandatory * Optional

// q3
let disjoint set1 set2 = not(Set.exists (fun x -> Set.exists(fun y -> x=y)set1 ) set2)
//disjoint Set[1;2;3] Set[4;5;6]

// q4
let sumEcts cs (cb:CourseBase) = 
                                let courses = Set.map (fun x-> Map.find x cb) cs
                                let (_,y)=List.unzip (Set.fold(fun l se -> se::l) [] courses)  // set to list                                           
                                List.fold (+) 0 y
// sumEcts Set[1;2;3] Map[1,("123",10);2,("123",5);3,("123",5)]

// q5
// A course group (man; opt) for a bachelor programme is valid for a given course base cb
// if:
// a: man and opt are disjoint,
// b: the sum of all mandatory ECTS points (i.e. the ECTS sum for all courses in man) less than or equal to 45,
// c: the set of optional courses opt is empty when the mandatory ECTS points add up 45, and
// d: the total number of ECTS points of mandatory and optional courses should be at least 45.
let valid (cg:CourseGroup) (cb:CourseBase)=
    let (man,opt)=cg
    let a=disjoint man opt
    let b = (sumEcts man cb)<=45
    let c = if (sumEcts man cb)>=45 && not ((Set.count opt)=0) then false else true
    let d = if (sumEcts man cb + sumEcts opt cb)>=45 then true else false
    a&&b&&c&&d

// valid (Set[1;2;3],Set[4;5;6]) Map[1,("123",10);2,("123",15);3,("123",5);4,("123",20);5,("123",5);6,("123",5)]

// q6
type BasicNaturalScience = CourseGroup
type TechnologicalCore = CourseGroup
type ProjectProfessionalSkill = CourseGroup
type Elective = CourseNo -> bool
type FlagModel = BasicNaturalScience * TechnologicalCore * ProjectProfessionalSkill * Elective
type CoursePlan = Set<CourseNo>

// A flag model (bns; tc; pps; ep) is valid if
// the three course groups bns; tc and pps are all valid,
// no course belongs to more than one of the course groups bns; tc and pps, and
// any course belonging to a course group bns; tc or pps must qualify as an elective course, that is, it must satisfy the predicate ep.
let CG_Disjoint bns tc pps = 
    let bnsUnion = Set.union (fst bns) (snd bns)
    let tcUnion =  Set.union (fst tc) (snd tc)
    let ppsUnion =  Set.union (fst pps) (snd pps)
    (disjoint bnsUnion tcUnion) &&
    (disjoint bnsUnion ppsUnion) &&
    (disjoint tcUnion ppsUnion);;

let isValid (fm:FlagModel) (cb:CourseBase) = 
    let (bns,tc,pps,ep)=fm
    let a = valid bns cb
    let b = valid tc cb
    let c = valid pps cb
    let d = CG_Disjoint bns tc pps
    let e = Set.forall (fun x -> ep x) (fst bns)
    let f = Set.forall (fun x -> ep x) (fst tc)
    let g = Set.forall (fun x -> ep x) (fst pps)
    a&&b&&c&&d&&e&&f&&g


// q7
// if the number of ECTS points earned from the courses in cs is 180, subject to the
// requirement that 45 points are earned in each course group of the flag model, including
// the elective courses.
let checkPlan (cs:CoursePlan) (fm:FlagModel) (cb:CourseBase) =
    isValid fm cb && //isvalid already checks than all courses are elective, and that each courseset is at least 45 points
    (sumEcts cs cb = 180)