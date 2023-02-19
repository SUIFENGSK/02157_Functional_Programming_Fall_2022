type FileSys = Element list
and Element = | File of string * string
              | Dir of string * FileSys;;

let d1 = Dir("d1",[File("a1","java");
         Dir("d2", [File("a2","fsx");
         Dir("d3", [File("a3","fs")])]);
         File("a4","fsx");
         Dir("d3", [File("a5","pdf")])]);;

let rec namesFileSys = function
   | [] -> []
   | e::es -> (namesElement e) @ (namesFileSys es)
and namesElement = function 
   | File (name,ext) -> [name+"."+ext]
   | Dir (name,fs) -> name::(namesFileSys fs)

// namesElement d1;;

// let rec searchFileSys extension es = 
//    match es with
//    | [] -> []
//    | e::es -> (searchElement extension e) @ (searchFileSys extension es)
// and searchElement extension = function 
//    | File (name,ext) -> if ext = extension then [name] else []
//    | Dir (name,fs) -> searchFileSys extension fs

let rec searchFileSys extension es = 
   match es with
   | [] -> Set.empty<string>
   | e::es -> Set.union (searchElement extension e) (searchFileSys extension es)
and searchElement extension = function
   | File (name,ext) -> if ext = extension then Set.singleton name else Set.empty<string>
   | Dir (_,fs) -> searchFileSys extension fs

// searchElement "fsx" d1

let rec longNamesFileSys es = 
   match es with
   | [] -> Set.empty<string>
   | e::es -> Set.union (longNamesElement e) (longNamesFileSys es)
and longNamesElement = function  
    | File (name,ext) -> Set.singleton (name+"."+ext)
    | Dir (name,fs) -> Set.map (fun x -> name + "\\" + x) (longNamesFileSys fs)

longNamesElement  d1