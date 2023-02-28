(**
   Langage intermédiaire Clj
   Similaire à Fun, mais sans fonctions anonymes.
   Les fonctions sont toutes définies au niveau global ("toplevel"),
   et attendent invariablement **un argument et une clôture**.
 *)

(* Distinction des variables en deux catégories *)
type var =
  | CVar of int    (* variable de clôture, désignée par un numéro *)
  | Name of string (* autres variables, désignées par leur nom    *)

(* Expressions, similaire à celles de Fun mais sans fonctions anonymes *)
type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of var
  | Unop  of Ops.unop * expression
  | Binop of Ops.binop * expression * expression
  | Tpl   of expression list
  | TplGet of expression * int

  | FunRef of string (* Référence au nom d'une fonction globale *)

  | App   of expression * expression
  | If    of expression * expression * expression
  | LetIn of string * expression * expression
  | LetRec of string * expression * expression
      
(* Définition d'une fonction globale *)
type function_def = {
  name: string; (* nom de la fonction, tel qu'il peut être désigné par FunRef *)
  code: expression;
  param: string; (* nom de l'unique paramètre *)
}

(* Un programme est défini par une expression et un ensemble de définitions
   de fonctions globales. *)
type program = {
  functions: function_def list;
  code: expression;
}
