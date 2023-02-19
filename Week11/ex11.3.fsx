let rec factSeq n=
    match n with
    | 0 -> Seq.singleton 1
    | _ -> Seq.append (factSeq(n-1)) (Seq.singleton (n*Seq.item(n-1)(factSeq(n-1))))

factSeq 4