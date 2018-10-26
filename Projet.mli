type code =
    Valeur of int
  | Plus of code * code
  | Moins of code * code
  | Mult of code * code
  | Div of code * code
type nombre = { valeur : int; expr : code; }
val nombre : int -> nombre
val nbr_of_lst : int list -> nombre list
val enlever : 'a -> 'a list -> 'a list
val combinaison : 'a list -> ('a * 'a * 'a list) list
val addition : nombre -> nombre -> nombre list -> nombre list list
val multiplication : nombre -> nombre -> nombre list -> nombre list list
val soustraction : nombre -> nombre -> nombre list -> nombre list list
val division : nombre -> nombre -> nombre list -> nombre list list
val appartient :
  nombre -> nombre -> (int -> int -> int) -> nombre list -> bool
val fus : 'a list list -> 'a list
val combin_abc :
  nombre -> nombre -> nombre list -> nombre list -> nombre list list
val flat_map : ('a -> 'b list) -> 'a list -> 'b list
val combin_ab_l : nombre list -> nombre list list
val a : nombre list
val cons : nombre list list
val construire : nombre list list -> nombre list list
val liste_sol : nombre list -> nombre list
val to_string : code -> string
val to_string_solution : nombre -> unit
val affich_combi_possibl : nombre list -> unit
val best : int -> nombre -> nombre -> nombre
val meilleure_approximation : int -> nombre list -> nombre
val trouve_sol : int -> nombre list -> nombre list
val foundp : int -> nombre list -> bool
val cher_val_aprox : int -> nombre list -> nombre -> nombre
val cher_val_aproxn : int -> nombre list list -> nombre -> nombre
val la_meilleure_combinaison : int -> nombre list -> nombre
exception Not_foundd
exception IllFormedExpression
exception Depassement
val solutions_exact : int -> nombre list -> nombre
val list_of_string : string -> int list
val input_of_string : string -> unit
val menu : unit -> 'a
