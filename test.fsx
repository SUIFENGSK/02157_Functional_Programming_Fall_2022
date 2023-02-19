// module Vector=
//  type Vector = V of float * float
//  let make(x,y) = V(x,y)
//  let coord(V(x,y)) = (x,y)
//  type Vector with
//     static member (~-) (V(x,y)) = V(-x,-y)
//     static member (+) (V(x1,y1),V(x2,y2)) = V(x1+x2,y1+y2)
//     static member (-) (V(x1,y1),V(x2,y2)) = V(x1-x2,y1-y2)
//     static member (*) (a, V(x,y)) = V(a*x,a*y)
//     static member (*) (V(x1,y1),V(x2,y2)) = x1*x2 + y1*y2
//  let norm(V(x,y)) = sqrt(x*x + y*y)

// let v1 = Vector.make(1.0,2.0)
// let v2 = Vector.make(3.0,4.0)
// let g = (+) v1 v2;;
type Fexpr =
| Const of float
| X
| Add of Fexpr * Fexpr
| Sub of Fexpr * Fexpr
| Mul of Fexpr * Fexpr
| Div of Fexpr * Fexpr;;
let rec compute x =
  function
  | Const r -> r
  | X -> x
  | Add(fe1,fe2) -> compute x fe1 + compute x fe2
  | Sub(fe1,fe2) -> compute x fe1 - compute x fe2
  | Mul(fe1,fe2) -> compute x fe1 * compute x fe2
  | Div(fe1,fe2) -> compute x fe1 / compute x fe2;;
  
compute 4.0 (Mul(X, Add(Const 1.0, X)));;


seq { for i in 1 .. 10 do yield i * i } 
