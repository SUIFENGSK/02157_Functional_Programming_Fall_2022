let rec sum (m,n)=
 if n=0 then m
 else m+n+sum(m,n-1)
sum(4,1)
