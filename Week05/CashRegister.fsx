type ArticleCode = string;;
type ArticleName = string;;
type Price       = int;;         // pr  where  pr >= 0
type Register    = (ArticleCode * (ArticleName*Price)) list;;

let reg = [("a1",("cheese",25));
           ("a2",("herring",4));
           ("a3",("soft drink",5)) ];;


type NoPieces    = int;;         // np  where np >= 0
type Item        = NoPieces * ArticleCode;;
type Purchase    = Item list;;

let pur = [(3,"a2"); (1,"a1")];;

type Info        = NoPieces * ArticleName * Price;;
type Infoseq     = Info list;;
type Bill        = Infoseq * Price;;

// let rec findArticle ac = function
//     | (ac',adesc)::_ when ac=ac' -> adesc
//     | _::reg                     -> findArticle ac reg
//     | _                          ->
//            failwith(ac + " is an unknown article code");;

let findArticle ac reg= List.tryFind(fun (ac',adesc) -> ac=ac') reg |> function
      | Some(ac',adesc) -> adesc
      | None ->failwith(ac + " is an unknown article code");;
findArticle "a1" reg

// let rec makeBill reg = function
//   | []           -> ([],0)
//   | (np,ac)::pur -> let (aname,aprice) = findArticle ac reg
//                     let tprice         = np*aprice
//                     let (billtl,sumtl) = makeBill reg pur
//                     ((np,aname,tprice)::billtl,tprice+sumtl);;

let makeBill (reg:Register) (pur:(int*string) list) = List.foldBack (fun (np,ac) (billtl,sumtl) -> 
                                                                     let (aname,aprice) = findArticle ac reg
                                                                     let tprice         = np*aprice
                                                                     ((np,aname,tprice)::billtl,tprice+sumtl)) pur ([],0);; 
makeBill reg pur;;