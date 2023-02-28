open Eimp
open Mips

let new_label =
  let count = ref 0 in
  fun () -> incr count; Printf.sprintf "__lab_%i" !count

let tr_fdef fdef =

  let return_label = new_label() in

  let rec tr_instr = function
    | Putchar r          -> move a0 r @@ li v0 11 @@ syscall
    | Read(rd, Global x) -> la rd x @@ lw rd 0 rd
    | Read(rd, Stack i)  -> lw rd i "$fp"
    | Write(Global x, r) -> la t0 x @@ sw r 0 t0
    | Write(Stack i, r)  -> sw r i "$fp"
    | Move(rd, r)        -> move rd r
    | Push r             -> 
      if r = "$s3" then 
        (*(List.fold_left (fun acc r -> acc @@ sw r 0 sp @@ subi sp sp 4) nop ["$s3";"$s4"] ) 
        @@ *) sw r 0 sp @@ subi sp sp 4
      else
        sw r 0 sp @@ subi sp sp 4
      
    | Pop n              -> addi sp sp (4*n)
    | Cst(rd, n)         -> li rd n
    | Unop(rd, Addi n, r)    -> addi rd r n
    | Unop(rd, Sll n, r)    -> sll rd r n
    | Unop(rd, Muli n, r)    -> muli rd r n
    | Binop(rd, Add, r1, r2) -> add rd r1 r2
    | Binop(rd, Mul, r1, r2) -> mul rd r1 r2
    | Binop(rd, Lt, r1, r2)  -> slt rd r1 r2
    | Call(f)            -> jal f
    | If(r, s1, s2) ->
      let elseLabel = new_label() in
      let endLabel = new_label() in

      beqz r elseLabel (*saut a else si r == 0*)
      @@ tr_seq s1 (*execution du if (non saute ssi r!=0)*)
      @@ b endLabel (*saut a la fin apres le if pour pas executer else *)
      @@ label elseLabel (*le else commence ici*)
      @@ tr_seq s2 (*code du else*)
      @@ label endLabel (*fin de la structure*)
      
    | While(s1, r, s2) ->
       let condiLabel = new_label() in
       let bodyLabel = new_label() in

       b condiLabel (*en premier on test la condition*)
       @@ label bodyLabel (*corps de la boucle*)
       @@ tr_seq s2
       @@ label condiLabel (*test de la condition apres un tour de boucle*)
       @@ tr_seq s1
       @@ bnez r bodyLabel (*on refait un tour si le test n'est toujours pas bon*)

    | Return -> 
      label return_label

  and tr_seq (s: Eimp.sequence) = match s with
    | Nop         -> nop
    | Instr i     -> tr_instr i
    | Seq(s1, s2) -> tr_seq s1 @@ tr_seq s2
  in

  (*convention d'appel, on sauvegarde tous les registres utilisés par la fonction pour les restaurer ensuite*)
  let prologue = sw fp 0 sp @@ subi sp sp 4 @@ sw ra 0 sp @@ subi sp sp 4 @@ addi fp sp 8 in
  let body = tr_seq fdef.code in
  let callee_save = List.fold_right (fun r acc -> acc @@ sw r 0 sp @@ subi sp sp 4) fdef.registers nop in
  let callee_restore = List.fold_right (fun r acc -> lw r 4 sp @@ addi sp sp 4 @@ acc) fdef.registers nop in
  let epilogue = li v0 0  @@ label return_label @@ callee_restore @@ move sp fp @@ lw ra (-4) fp @@ lw fp 0 fp @@ jr ra in

  prologue @@ callee_save @@ body @@ epilogue

let tr_prog prog =
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ label "init_end"
    @@ move a0 v0
    (* @@ sw v0 0 sp
     * @@ subi sp sp 4 *)
    @@ jal "main"
    (* Après l'exécution de la fonction "main", appel système de fin de
       l'exécution. *)
    @@ li v0 10
    @@ syscall
  and built_ins =
    (* Fonction de conversion chaîne -> entier, à base d'une boucle sur les
       caractères de la chaîne. *)
    comment "built-in atoi"
    @@ label "atoi"
    @@ li   v0 0
    @@ label "atoi_loop"
   (* @@ lbu  t0 0 a0*)       (* cette instruction empechait la fonction de fonctionner*)
    @@ beqz t0 "atoi_end"
    @@ addi t0 t0 (-48)
    @@ bltz t0 "atoi_error"
    @@ bgei t0 10 "atoi_error"
    @@ muli v0 v0 10
    @@ add  v0 v0 t0
    @@ addi a0 a0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ jr   ra
  in

  (**
     Code principal pour générer le code MIPS associé au programme source.
   *)
  let function_codes = List.fold_right
    (fun fdef code ->
      label fdef.name @@ tr_fdef fdef @@ code)
    prog.functions nop
  in
  let text = init @@ function_codes @@ built_ins
 
  and data = List.fold_right (* les variables globales sont stockees dans la partie data*)
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals nop
  in
  
  { text; data }
  
