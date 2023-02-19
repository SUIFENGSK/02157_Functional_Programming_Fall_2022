// Two solutions to the long names part of the File System Exercise
//                                 Michael R. Hansen     02-11-2021

type FileSys = Element list
and Element  = | File of string * string
               | Dir of string * FileSys;;



let d1 = Dir("d1",[File("a1","java");
                   Dir("d2", [File("a2","fsx");
                              Dir("d3", [File("a3","fs")])]);
                   File("a4","fsx");
                   Dir("d3", [File("a5","pdf")])]);;


let addDir d lns = Set.map (fun ln -> d + "\\" + ln) lns 

let rec longNamesElement1 =
      function
      | File(n,ext) -> Set.singleton(n+"."+ext)
      | Dir(d,fs)   -> let lns = longNamesFileSys1 fs
                       addDir d lns 
and longNamesFileSys1 = 
      function 
      | []    -> Set.empty
      | e::es -> Set.union (longNamesElement1 e) (longNamesFileSys1 es);;


let rec lNE path =
      function
      | File(n,ext) -> Set.singleton(path + n+ "."+ext)
      | Dir(d,fs)   -> lNFS (path + d + "\\") fs
and lNFS path fs = 
      List.fold (fun lns e -> Set.union lns (lNE path e)) Set.empty fs;;

let longNamesElement2 e = lNE "" e;; 
let longNamesFileSys2 fs = lNFS "" fs;;

   

longNamesElement1 d1;;  

       