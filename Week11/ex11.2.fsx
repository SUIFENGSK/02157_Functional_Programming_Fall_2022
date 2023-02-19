let rec factHelper n=
   match n with
   | 0 -> 1
   | _ -> n*factHelper(n-1)

// fracHelper 3

let frac=Seq.initInfinite (fun x-> factHelper x)

frac