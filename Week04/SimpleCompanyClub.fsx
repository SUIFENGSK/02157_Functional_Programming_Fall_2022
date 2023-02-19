type memberName = string
type telephoneNumber = int
type yearBirth = int
type ths = string list
type clubMember = memberName * (telephoneNumber * yearBirth * ths)
type members = clubMember list
type arrangement = telephoneNumber*yearBirth*ths
type targetGroup = memberName*telephoneNumber list

let p1:arrangement = (123456789, 1982, ["soccer"; "jazz"]) 
let p2:arrangement = (123456789, 1982, ["soccer"])

let reg:members=[("John",(123123, 1990, ["soccer"; "jazz"]));("Mary",(11123, 1999, ["soccer"]));("Peter",(9999, 2000, ["jazz"]))]

let getMemberName (m:clubMember) : string = 
    match m with
    | (name,_) -> name

let getMemberTel (m:clubMember) : telephoneNumber =
    match m with
    | (_, (tel,_,_)) -> tel

let rec findMemberThs (name:memberName) (reg:members) : ths =
    match reg with
    | [] -> []
    | (n,(t,y,ths))::xs -> if n=name then ths else findMemberThs name xs

let rec findMemberYearBirth (name:memberName) (reg:members) : yearBirth =
    match reg with
    | [] -> 0
    | (n,(t,y,ths))::xs -> if n=name then y else findMemberYearBirth name xs

let getArrangementYearBirth (p:arrangement) : yearBirth = 
    match p with
    | (t,y,ths) -> y

let getArrangementThs (p:arrangement): ths = 
    match p with
    | (t,y,ths) -> ths

let rec checkThs (ths1:ths) (ths2:ths) : bool =
    match ths2 with
    | [] -> true
    | x::xs -> List.contains x ths1 && checkThs ths1 xs
checkThs (findMemberThs "Peter" reg) (getArrangementThs p1)

let convertToTargetGroup (m:clubMember) : targetGroup = 
    match m with
    | (name,(t,y,ths)) -> (name,[t])

let rec extractTargetGroup (p:arrangement) (m:members) : targetGroup list = 
    match m with
    | [] -> []
    | selectedMember::m -> if ((findMemberYearBirth (getMemberName selectedMember) reg > getArrangementYearBirth p) && (checkThs (findMemberThs (getMemberName selectedMember) reg) (getArrangementThs p1))) 
                              then convertToTargetGroup selectedMember::extractTargetGroup p m 
                           else extractTargetGroup p m
extractTargetGroup p1 reg