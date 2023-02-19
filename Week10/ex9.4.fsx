let rec getListLength =
   function
   | ([],l) -> l
   | (s,l) -> getListLength(s.Tail,l+1)

getListLength([1;2;3;4],0)