let jud_pop = [("Alba", 342376); ("Arad", 430629); ("Arges", 612431); ("Bacau", 616168); ("Bihor", 575398); ("Bistrita-Nasaud", 286225); ("Botosani", 412626); ("Brasov", 549217); ("Braila", 321212); ("Buzau", 451069); ("Caras-Severin", 295579); ("Calarasi", 306691); ("Cluj", 691106); ("Constanta", 684082); ("Covasna", 210177); ("Dambovita", 518745); ("Dolj", 660544); ("Galati", 536167); ("Giurgiu", 281422); ("Gorj", 341594); ("Harghita", 310867); ("Hunedoara", 418565); ("Ialomita", 274148); ("Iasi", 772348); ("Ilfov", 388738); ("Maramures", 478659); ("Mehedinti", 265390); ("Mures", 550846); ("Neamt", 470766); ("Olt", 436400); ("Prahova", 762886); ("Satu Mare", 344360); ("Salaj", 224384); ("Sibiu", 397322); ("Suceava", 634810); ("Teleorman", 380123); ("Timis", 683540); ("Tulcea", 213083); ("Vaslui", 395499); ("Valcea", 371714); ("Vrancea", 340310)];;

(* 1 *)

let rec judete lista = match lista with
| [] -> 0
| head :: tail -> 1 + judete tail;;

judete jud_pop;;

(* 2 *)

let modif nume x listaa = List.map(fun (judet,populatie) -> if nume = judet then ( nume, populatie + x )  else ( judet, populatie ) ) listaa;;

let rez = modif "Alba" 4 jud_pop;;

(* 3 *)

let medie llist = List.fold_left ( fun sum (judet , populatie) -> sum + (populatie / judete llist )) 0 llist;; (* am folosit functia judete de la problema 1 *) 

medie jud_pop;;

(* 4 *)

let statistica judett x lstt = List.filter ( fun ( judet , populatie) -> populatie > x ) lstt;;

let rez = statistica "Alba" 342376 jud_pop;;

