// question 1
let rec collatz n = seq {yield n; if n=1 then () else yield! collatz (if n%2=0 then n/2 else 3*n+1)}
collatz 10