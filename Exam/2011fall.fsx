// Problem 2
type exp = | C of int
           | BinOp of exp * string * exp
           | Id of string
           | Def of string * exp * exp;;

// q1
let a1 = C(1)
let a2 = BinOp(C(1),"+",C(2))
let a3 = BinOp(C(2),"*",C(2))

// q2
let rec toString e =
    match e with
    | C c -> string c
    | BinOp(e1,s,e2) -> "(" + toString e1 + s + toString e2 + ")"

// toString (BinOp(C(3),"+",(BinOp(C(5),"*",C(2)))))

// q3
let rec extractOperators e =
    match e with
    | C c -> Set.empty<string>
    | BinOp(e1,s,e2) -> Set.add s (Set.union (extractOperators e1) (extractOperators e2))

// extractOperators (BinOp(C(3),"+",(BinOp(C(5),"*",C(2)))))

// q4
let getIdAndExpr e =
    match e with
    | Def(id,_,e1) -> (id,e1)

let isDef e =
    let id = fst (getIdAndExpr e)
    let expr = snd (getIdAndExpr e)
    let rec test expr =
        match expr with
        | C c -> false
        | BinOp(e1,s,e2) -> test e1 && test e2
        | Id id1 -> id = id1
    test expr

isDef (Def("x",C(1),BinOp(Id("y"),"+",Id("x"))))