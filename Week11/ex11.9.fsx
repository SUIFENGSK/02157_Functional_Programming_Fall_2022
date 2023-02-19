let gseq n =
   let rec gseqHelper n =
       match n with
       | 0 -> Seq.singleton 0
       | _ -> Seq.append (Seq.singleton n)(Seq.append (Seq.singleton -n) (gseqHelper (n-1)))
   Seq.rev (gseqHelper n)

gseq 5