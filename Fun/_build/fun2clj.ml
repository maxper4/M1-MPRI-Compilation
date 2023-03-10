(**
   Traduction de Fun vers Clj ("closure conversion")
 *)

(* Module pour manipuler des ensembles de variables *)
module VSet = Set.Make(String)

let translate_program e =
  (* Liste des définitions de fonctions globales *)
  let fdefs = ref [] in
  (* Fonction générant un nom de fonction unique, à utiliser lors de la
     création d'une fonction globale Clj à partir d'une fonction anonyme Fun *)
  let new_fname =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "fun_%i" !cpt
  in
  
  (* Traduction d'une expression.
     Paramètres :
     - e     : l'expression à traduire
     - bvars : l'ensemble des variables liées (pour identifier les variables
               libres, qu'il faudra ajouter à la clôture de la fonction
               englobante)

     Résultat : paire (e', l)
     - e' : l'expression traduite
     - l  : une liste d'associations entre variables libres et les numéros
            correspondant dans la clôture
   *)
  let rec tr_expr (e: Fun.expression) (bvars: VSet.t): Clj.expression * (string * int) list =
    (* Liste d'associations entre variables libres et numéros dans la clôture *)
    let cvars = ref [] in
    (* Fonction à appeler à chaque fois que l'on trouve une nouvelle variable
       libre, pour
       - l'enregistrer dans cvars
       - et renvoyer son numéro *)
    let new_cvar =
      let cpt = ref 0 in (* commencera à 1 *)
      fun x -> incr cpt; cvars := (x, !cpt) :: !cvars; !cpt
    in
    
    (* Traduction d'une variable, avec ajout à la clôture si nécessaire *)
    let rec convert_var x bvars =
      Clj.(if VSet.mem x bvars
        then Name(x) (* si variable liée : on conserve son nom *)
        else if List.mem_assoc x !cvars
        then CVar(List.assoc x !cvars) (* si variable libre déjà enregistrée : on donne son numéro *)
        else       
          CVar(new_cvar x))         (* sinon, création et enregistrement d'un nouveau numéro *)
        
    (* Traduction d'une expression.
       Renvoie l'expression traduite, et étend la clôture à la volée *)
    and crawl e bvars = match e with
      | Fun.Cst(n) ->
        Clj.Cst(n)

      | Fun.Bool(b) ->
        Clj.Bool(b)
          
      | Fun.Var(x) -> (* Traduction d'une variable déléguée à convert_var *)
        Clj.Var(convert_var x bvars)

      | Fun.Unop(op, e) ->
        Clj.Unop(op, crawl e bvars)
          
      | Fun.Binop(op, e1, e2) ->
        Clj.Binop(op, crawl e1 bvars, crawl e2 bvars)

      | Fun.Tpl el ->
        Clj.Tpl (List.map (fun e -> crawl e bvars) el)

      | Fun.TplGet (e, i) ->
        Clj.TplGet (crawl e bvars, i)

      (* La portée du 'x' de 'let x = e1 in e2' est l'expression e2.
         Lors de la traduction de e2 on ajoute donc x aux variables liées. *)
      | Fun.LetIn(x, e1, e2) ->
        Clj.LetIn(x, crawl e1 bvars, crawl e2 (VSet.add x bvars))

      | Fun.LetRec (x, e1, e2) ->
        Clj.LetRec (x, crawl e1 (VSet.add x bvars), crawl e2 (VSet.add x bvars))

      (* Traduction d'une fonction anonyme *)
      | Fun.Fun(x, e) ->
         (* créer une définition de fonction, et l'ajouter à la référence fdefs *)
         (* pour cela, créer un nouveau nom avec new_fname *)
         (* renvoyer une expression créant une clôture fonctionnelle *)
         (* note : une clôture peut être vue comme un n-uplet *)
         let fname = new_fname () in         
         (* only x is bounded in sub function, other are in closure*)
        let (e', cvars') = tr_expr e (VSet.singleton x) in

        let fdef = Clj.{name=fname; code=e'; param=x} in
        fdefs := fdef :: !fdefs;

         (* sort closure variable to not mix them up *)
        let cvars' = List.sort (fun (x, i) (y, j) -> compare i j) cvars' in

        (* add values of bounded variable for asked closure variables*)
        Clj.Tpl(Clj.FunRef(fname) :: List.map (fun (y, i) -> Clj.Var(convert_var y bvars)) cvars')

      | Fun.App(e1, e2) ->
        Clj.App(crawl e1 bvars, crawl e2 bvars)

      | Fun.If(e1, e2, e3) ->
        Clj.If(crawl e1 bvars, crawl e2 bvars, crawl e3 bvars)

    in
    (* On renvoie le résultat de la traduction de e, et la liste des variables de clôture *)
    let te = crawl e bvars in
    te, !cvars

  in

  (* Traduction du programme complet : on conserve le code traduit et les
     fonctions globales créées à la volée. On part de l'ensemble vide pour
     les variables liées. *)
  (* Note : on ignore les 'variables de clôture', puisqu'on n'est pas dans
     une clôture. On pourrait utiliser une variante de tr_expr ne mentionnant
     ni bvars ni cvars. *)
  let code, _ = tr_expr e VSet.empty in
  Clj.({
    functions = !fdefs;
    code = code;
  })
