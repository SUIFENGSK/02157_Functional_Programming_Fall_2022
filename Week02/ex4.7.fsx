let rec multiplicity x xs =
   match xs with
   | [] -> 0
   | t::tail when t=x -> 1+multiplicity x tail // t==x add 1 and remove t
   | t::tail -> multiplicity x tail;; // t!=x remove t

multiplicity 2 [1;2;3;4;2;2];;
multiplicity 2 [1;2;8;2];;