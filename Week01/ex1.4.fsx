let rec f n = 
   if n=1 then 1 
   else n + f(n-1)
f(4)