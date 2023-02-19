let rec remove y xs =
    match xs with
      | [] -> []
      | x::tail when x=y -> remove y tail
      | x::tail -> x::remove y tail;;
