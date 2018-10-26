
(*******        **********
********le type***********)

type code = Valeur of int | Plus of code *code | Moins of code *code| Mult of code*code | Div of code *code;;

type nombre= { valeur: int; expr: code };;

(*********** int_to_type_nombre ************)

let nombre x= { valeur=x; expr=Valeur x; };;
let nbr_of_lst = fun l->
  List.map (fun x -> nombre x) l;;

let rec enlever x l= match l with
[] -> []
| a::q -> if a=x then q else a::(enlever x q);;

(*****combinaisons********)
(*1*)

let combinaison = fun liste ->
  let tmp = liste in 
  let rec aux = fun liste ->
  match liste with
  |[]-> []
  | x::ys->( List.map (function y-> ( x,y,( enlever x (enlever y tmp)) ) ) ys  )@ aux ys  in
  aux liste  ;;

(***  operations div mult addi sous ***)
(*2**)
let addition n1 n2 l=
  [[{valeur=n1.valeur+n2.valeur; expr=Plus(n1.expr,n2.expr)}]]@[l];;

let multiplication n1 n2 l=
  [[{valeur=n1.valeur*n2.valeur; expr=Mult(n1.expr,n2.expr)}]]@[l];;

let soustraction n1 n2 l=
  [[{valeur=n1.valeur-n2.valeur; expr=Moins(n1.expr,n2.expr)}]]@[l];;

let division n1 n2 l=
  [[{valeur=n1.valeur/n2.valeur; expr=Div(n1.expr,n2.expr)}]] @[l];;
let appartient n1 n2 f l =
  List.exists (fun x->x.valeur=(f n1.valeur n2.valeur)) l;;


(***combiner les entiers   tout en evitant les operations inutiles*****)
(*3***)


 let fus l=(List.hd(l))@(if (List.length(List.tl l)!=0) then  List.hd(List.tl l) else []) ;;
let rec combin_abc a b l acc =

    if not (appartient a b (+) acc) then
      fus (addition a b l) :: combin_abc  a b l (List.hd (addition a b l)@acc) 
    else
    if a.valeur !=1 && b.valeur !=1 && not (appartient a b ( * ) acc) 
    then
      fus  (multiplication a b l)::  combin_abc a b l (List.hd(multiplication a b l)@acc) 
    else
    if a.valeur > b.valeur && not (appartient a b (-) acc) 
    then
      fus (soustraction a b l)::  combin_abc a b l (List.hd(soustraction a b l)@acc) 
    else
    if b.valeur > a.valeur && not (appartient b a (-) acc) 
    then
      fus  (soustraction b a l)::  combin_abc a b l (List.hd(soustraction b a l)@acc) 
    else
    if b.valeur!=1 && (a.valeur mod b.valeur = 0) && not (appartient a b (/) acc) 
    then
      fus  (division a b l)::  combin_abc a b l (List.hd(division a b l)@acc) 
    else
    if a.valeur!=1 && (b.valeur mod a.valeur = 0) && not (appartient b a (/) acc)  
    then
      fus (division b a l)::  combin_abc a b l (List.hd(division b a l)@acc) 
 
	 else [];;

 combin_abc (nombre 1) (nombre 2) ([nombre 3]) [] ;;
(* **function flat_map  pour eviter de reecrire ** *)

let flat_map f x = List.flatten(List.map f x);;



(*****appliquer combine_abc a tous les combinaisons possibles *****)

let combin_ab_l l= flat_map (fun (a,b,t) -> combin_abc a b t [] ) (combinaison l);;

let a = [nombre 2 ;nombre 4 ;nombre 3;nombre 7;nombre 25 ];;
  let cons =combin_ab_l a;;


(*** methode qui permer de construire toute les combinaisons **************************)
let rec construire l =
  let rec construire_aux l1=
 
    match l1 with
    | []->[]
    |a::b::[]->combin_abc a b [] [] 
    |a::b::t->construire ((combin_abc a b t []))  in
  match l with
    []->[]
  |h::t-> (construire_aux h) @construire t ;;

construire cons ;;

(** lister les combinaisons   *)


let liste_sol a=
  (List.map (List.hd) (construire (combin_ab_l a)));;

(** affichage *)
let to_string =
  let rec loop p1 p2 = function
    | Valeur x -> string_of_int x
    | Plus (x, y) -> Printf.sprintf "%s%s + %s%s"  p1 (inner x) (inner y) p2
    | Moins (x, y) -> Printf.sprintf "%s%s - %s%s" p1 (inner x) (inner y) p2
    | Mult (x, y) -> Printf.sprintf "%s%s * %s%s"  p1 (inner x) (inner y) p2
    | Div (x, y) -> Printf.sprintf "%s%s / %s%s"   p1 (inner x) (inner y) p2
  and inner expr = loop "(" ")" expr in loop "" "";;

let to_string_solution =fun {valeur=valeur; expr=expr}->
  Printf.printf "une valeur = %d  correspondant a l'expression  \" %s \"" valeur (to_string expr);print_newline();;

(** afficher les combin **)
let affich_combi_possibl l=
let rec aux l=
  match l with
  |[]->print_string ""
  |h::t->to_string_solution h; aux t in aux (liste_sol l) ;;

(*affich_combi_possibl [nombre 1 ;nombre 2 ;nombre 3];;*)


(**Part2===>******Meilleure approximation **********)
(*****fonction qui calcule la valeur la plus proche  a n entre a et b ****)

let best = fun n a b -> if abs (a.valeur - n ) >= abs( b.valeur - n )  then b else  a ;;

(* *******appliquer la fonction best  a n element "une liste" ******* *)
(*methode permer de donner la meilleure aproximation en combinons exactement tous les elements de la liste 
let meilleure_approximation l n =
  List.fold_right (fun a b -> best n a b )(liste_sol l) ( List.hd (liste_sol l) ) ;;
*)

let rec meilleure_approximation = fun n l ->
  List.fold_right (fun a b -> best n a b  ) l ( List.hd l ) ;;



let trouve_sol n l= List.filter (function x->x.valeur=n) l;;
let foundp n l= [] <> trouve_sol n l;;


let rec cher_val_aprox n l res=
match (trouve_sol n l) with
[] ->  cher_val_aproxn n (combin_ab_l l) (meilleure_approximation n (res::l))
| sol1::_ -> sol1
and  cher_val_aproxn n ls res=
match ls with
[] -> res
| l1::tl -> let sol1= cher_val_aprox n l1 res in
if (foundp n [sol1] ) then sol1
else  cher_val_aproxn n tl (best n sol1 res);;

let la_meilleure_combinaison n l= cher_val_aprox  n l (List.hd l);;




to_string_solution(la_meilleure_combinaison 1999 [nombre 4;nombre 2;nombre 7;nombre 3;nombre 25]);;

(* ****Part3***** resultat exact ********* *)
exception  Not_foundd;;
exception IllFormedExpression;;
exception Depassement;;

let solutions_exact n l = let v=la_meilleure_combinaison n l in if v.valeur =n then v else raise Not_foundd;;

(* to_string_solution(solutions_exact 2000 a);;*)
(* menu principale *)


(** methode transforme un string en une liste *)
(* let rec list_of_string s =
 
  let rec aux s=
    if String.length s =0 then [] else if
	s.[0]!=';' then (int_of_string (Char.escaped s.[0]))::aux( String.sub s 1 (String.length s -1) )
  else  aux (String.sub s 1 (String.length s -1))
in aux  (String.sub s 1 ((String.length  s) -2));;

*)
#load "str.cma";;

let list_of_string = fun s->
  List.map (fun x->int_of_string x)( Str.split (Str.regexp ";") (String.sub s 1 ((String.length  s) -2) ) ) ;;

list_of_string "[2;4;3;7;25]";;

let input_of_string c  =
if String.length c > 0
  then
    match c  with
    | "1" ->  begin  print_string "Entrer une liste de depart de taille inferrieur a 4 svp " ;
            let l = nbr_of_lst(list_of_string(read_line())) in
		if List.length l >9 then raise Depassement else
            affich_combi_possibl  l 
end
    | "2" -> begin
	 print_string "Entrer un entier a evaluer :  " ;
	 let n = (int_of_string(read_line())) in
	 print_string "entrer la liste de valeurs de depart :  " ;
         let l = nbr_of_lst(list_of_string(read_line())) in
         to_string_solution (la_meilleure_combinaison n l)
end
    |"3" -> begin
	 print_string " Entrer un entier a evaluer :  " ;
	 let n = (int_of_string(read_line())) in
	 print_string "entrer la liste de valeurs de depart :  " ;
	 let l = nbr_of_lst(list_of_string(read_line())) in
         to_string_solution (solutions_exact n l )
end
    | "4" -> exit (0)
    else raise IllFormedExpression;;



let  menu () = 
let rec aux() =
 print_string "######## MENU ######## \n 
#####     1 - Afficher toutes les combinaisons possibles           #####  \n
#####     2 - la_meilleure_combinaison                             #####  \n
#####     3 - resultat exact                                       #####  \n
#####     4 - exit                                                 #####  \n ";
let rep = read_line()
in
try (input_of_string rep) ; aux() with 
                            |Not_foundd ->print_string "\n *** Erreur :AUCUN RESULTAT TROUVÉ ***\n\n";aux()
                            |Depassement->  print_string "\n *** Erreur : lISTE TROP GRANDE ***\n\n";aux()   
			    |autre ->print_string"\n *** Erreur : ERREUR DE SAISIE ! ***\n\n" ;aux()
				
in aux()  

;;


menu();;







