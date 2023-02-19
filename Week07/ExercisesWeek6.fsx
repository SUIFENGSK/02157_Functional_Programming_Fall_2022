//Exercises involving analysis and construction of list-based programs
//Task 1
let rec collect f = function
| [] -> []
| x::xs -> f x @ collect f xs;;
collect (fun (a,b) -> [a..b]) [(1,3); (4,7); (8,8)];;

// Give an argument showing that this type is indeed the most general type of collect. That is, any other type for collect is an instance of the most general type.
// check slides

// collect (fun (a,b) -> [a..b]) [(1,3); (4,7); (8,8)]
// collect f [(1,3); (4,7); (8,8)]
// f (1,3) @ collect f [(4,7); (8,8)]
// [1;2;3] @ (f (4,7) @ collect f (8,8))
// [1;2;3] @ ([4;5;6;7] @ (f (8,8) @ collect f()))
// [1;2;3] @ ([4;5;6;7] @ ([8] @ []))
// [1;2;3] @ ([4;5;6;7] @ [8])
// [1;2;3] @ [4;5;6;7;8]
// [1;2;3;4;5;6;7;8]

// ((int*int) -> int list) -> (int*int) list -> int list

// Luggage and Flights
type Lid = string
type Flight = string
type Airport = string
type Route = (Flight * Airport) list
type LuggageCatalogue = (Lid * Route) list

let rec findRouteHelp lid luggageCatalogue =
        match luggageCatalogue with
        | []->[]
        | (a,b)::luggageCatalogue when a=lid -> b @ findRouteHelp lid luggageCatalogue
        | _ -> []
let findRoute lid luggageCatalogue = let route = findRouteHelp lid luggageCatalogue
                                     if route.Length = 0 then failwith ("no found")
                                     else route

findRoute "DL 016-914" [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]);("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])]

let rec inRoute flight route=
       match route with
       | []->false
       | (a,_)::route -> if a=flight then true else inRoute flight route

inRoute "DL 189" [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]

let rec withFlight f lc=
       match lc with
       | []->[]
       | (lid,route)::lc -> if (inRoute f route) then lid::withFlight f lc else withFlight f lc
withFlight "DL 124" [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]);("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])]

type ArrivalCatalogue = (Airport * Lid list) list
let extend (lid,route,ac) : ArrivalCatalogue = 
    
    let rec addLidToAirport (a:Airport) (id:Lid) (ac:ArrivalCatalogue) : ArrivalCatalogue =
        match ac with
        | (airport,idList)::tail when airport = a -> (airport,id::idList)::tail
        | head::tail -> head::addLidToAirport a id tail
        | [] -> [(a,[id])]

    let rec addRouteToCatalogue (r:Route) (ac:ArrivalCatalogue) : ArrivalCatalogue =
        match r with
        | (_,ap)::tail -> addLidToAirport ap lid (addRouteToCatalogue tail ac)
        | [] -> ac

    addRouteToCatalogue route ac

extend ("DL 016-914", [("DL 189","ATL"); ("DL 124","PEK"); ("SN 733","CPH")], [("ATL",["DL 016-915"]); ("BRU",["DL 016-915"]); ("CPH",["DL 016-915"])])

let toArrivalCatalogue (luggageCatalogue:LuggageCatalogue) : ArrivalCatalogue = List.fold (fun a b -> extend (fst b, snd b, a)) [] luggageCatalogue
toArrivalCatalogue [("DL 016-914", [("DL 189","ATL"); ("DL 124","BRU"); ("SN 733","CPH")]);("SK 222-142", [("SK 208","ATL"); ("DL 124","BRU"); ("SK 122","JFK")])]
