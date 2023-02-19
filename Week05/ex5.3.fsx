//Solve Exercise 4.12 using List.fold or List.foldBack.

//4.12
let rec sum p xs =
    match xs with
    | [] -> 0
    | x::xs -> if p x then x + sum p xs else sum p xs;;
let p = fun x -> x > 0;;
sum p [-1;2;3;4];;

//5.3
let sumFold p xs = List.fold (fun a b -> a+b) 0 (List.filter p xs)
let sumFoldBack p xs = List.foldBack(fun a b -> a+b) (List.filter p xs) 0
sumFold (fun x -> x>0) [-1;2;3;4]
sumFoldBack (fun x -> x>0) [-1;2;3;4]