let f n =
   match n with
   | _ when (n%2=0 || n%3=0) && (not(n%5=0)) -> true
   | _ -> false;;

f(24);;
f(27);;
f(29);;
f(30);;