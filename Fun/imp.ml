(**
   Langage Imp étendu
   On a en plus la possibilité de manipuler des pointeurs, sur des données
   comme sur des fonctions.
 *)

type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Unop  of Ops.unop * expression
  | Binop of Ops.binop * expression * expression
  | Call  of string * expression list
  (* Pointeurs *)
  | Deref of expression (* lecture à une adresse mémoire *)
  | Addr  of string     (* & -> donne l'adresse d'une variable, utilisé ici
                           pour obtenir un pointeur de code sur une fonction *)
  | PCall of expression * expression list (* appel de fonction par pointeur *)
  | Sbrk  of expression (* primitive d'allocation dans le tas *)
      
type instruction =
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
  (* Pointeurs *)
  | Write   of expression * expression (* écriture à une adresse mémoire *)
      
and sequence = instruction list

(* On ne donne pas de primitives pour manipuler des tableaux, car on peut se
   ramener à de simples manipulations de pointeurs. Les quatre fonctions 
   ci-dessous peuvent être vues comme des macros pour cela. *)
let array_access (t: expression) (i: expression): expression =
  (* Expression calculant le pointeur d'accès à la case i du tableau t *)
  Binop(Ops.Add, t, Binop(Ops.Mul, i, Cst 4))
let array_get (t: expression) (i: expression): expression =
  (* Expression de lecture t[i] *)
  Deref(array_access t i)
let array_set (t: expression) (i: expression) (e: expression): instruction =
  (* Instruction d'écriture t[i] = e *)
  Write(array_access t i, e)
let array_create (n: expression): expression =
  (* Allocation d'un tableau de n cases *)
  Call("malloc", [Binop(Mul, n, Cst 4)])
    
type function_def = {
  name: string;
  code: sequence;
  params: string list;
  locals: string list;
}
    
type program = {
  (* variation mineure par rapport à IMP : on ajoute une séquence
     d'instructions principale au lieu d'une fonction main *)
  main: sequence;
  functions: function_def list;
  globals: string list;
}

(* Fusion de plusieurs programmes.
   Utilisée pour l'inclusion de bibliothèques. *)
let merge lib prog = {
  main = lib.main @ prog.main;
  functions = lib.functions @ prog.functions;
  globals = lib.globals @ prog.globals;
}
