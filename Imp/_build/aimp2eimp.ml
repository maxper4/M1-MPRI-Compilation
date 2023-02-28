open Eimp
open Register_allocation

let tr_unop = function
  | Aimp.Addi n -> Addi n
  | Aimp.Sll n -> Sll n
  | Aimp.Muli n -> Muli n
let tr_binop = function
  | Aimp.Add -> Add
  | Aimp.Mul -> Mul
  | Aimp.Lt -> Lt

let dst_reg = "$t0"
let op1_reg = "$t0"
let op2_reg = "$t1"

let tr_fdef fdef =
  let alloc, mx = allocation fdef in

  let registres_reels = [
    "$v0"; "$v1"; "$a0"; "$a1"; "$a2"; "$a3";
    "$t0"; "$t1"; "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7"; "$t8"; "$t9";
    "$s0"; "$s1"; "$s2"; "$s3"; "$s4"; "$s5"; "$s6"; "$s7";
    "$sp"; "$fp"; "$ra"
  ] in

  let save vr = match Graph.VMap.find vr alloc with
    | Actual r  -> Nop
    | Stacked i -> Instr(Write(Stack(-i-2), dst_reg))
  in
  let load op vr = if List.mem vr registres_reels then Nop else 
    match Graph.VMap.find vr alloc with
    | Actual r  -> Nop
    | Stacked i -> Instr(Read(op, Stack(-i-2)))
  in
  let load1 = load op1_reg in
  let load2 = load op2_reg in
  let reg op vr = if List.mem vr registres_reels then vr else
    match Graph.VMap.find vr alloc with
    | Actual r  -> r
    | Stacked i -> op
  in
  let dst = reg dst_reg in
  let op1 = reg op1_reg in
  let op2 = reg op2_reg in

  let rec find_index e list = match list with
  | [] -> failwith "Not found"
  | h::t -> if h = e then 0 else 1 + find_index e t 
in

  (* On utilise les registres réels quand il y en a, et à défaut $t0 et $t1. *)
  let rec tr_instr = function
    | Aimp.Putchar vr ->
       load1 vr
       @@ Instr(Putchar(op1 vr))
    | Aimp.Read(vrd, x) ->
        if List.mem x Aimp.(fdef.locals) then 
          load1 x @@ Instr(Move(op2 vrd, op1 x))
        else if List.mem x Aimp.(fdef.params) then
            Instr(Read(dst vrd, Stack(4 * (find_index x Aimp.(fdef.params) + 1))))
        else 
          Instr(Read(dst vrd, Global(x)))
    | Aimp.Write(x, vr) ->
        if List.mem x Aimp.(fdef.locals) then 
        Instr(Move(dst x, op1 vr)) @@ save x
        else if List.mem x Aimp.(fdef.params) then
          Instr(Write(Stack(4 * (find_index x Aimp.(fdef.params) + 1)), dst vr)) @@ save vr
        else if List.mem vr registres_reels then
          Instr(Move(x, dst vr))
        else 
          Instr(Write(Global(x), dst vr)) @@ save vr
    | Aimp.Move(vrd, vr) ->
       load1 vr @@ Instr(Move(dst vrd, op1 vr)) @@ save vrd
    | Aimp.Push vr ->
         load1 vr @@ Instr(Push(op1 vr))
    | Aimp.Pop n ->
        if n = 0 then Nop else Instr(Pop n)
    | Aimp.Cst(vrd, n) ->
       Instr(Cst(dst vrd, n))
       @@ save vrd
    | Aimp.Unop(vrd, op, vr) ->
        load1 vr
       @@ Instr(Unop(dst vrd, tr_unop op, op1 vr))
       @@ save vrd
    | Aimp.Binop(vrd, op, vr1, vr2) ->
         load1 vr1
         @@ load2 vr2
         @@ Instr(Binop(dst vrd, tr_binop op, op1 vr1, op2 vr2))
         @@ save vrd
    | Aimp.Call(f, n) ->
        Instr(Call(f))
    | Aimp.If(vr, s1, s2) ->
        load1 vr
        @@ Instr(If(op1 vr, tr_seq s1, tr_seq s2))
    | Aimp.While(s1, vr, s2) ->
        Instr(While(tr_seq s1, op1 vr, tr_seq s2))
    | Aimp.Return ->
        Instr(Return)

  and tr_seq = function
    | Aimp.Seq(s1, s2) -> Seq(tr_seq s1, tr_seq s2)
    | Aimp.Instr(_, i) -> tr_instr i
    | Aimp.Nop         -> Nop

  in

  let pushed = 
 Graph.VMap.fold (fun c vr acc -> 
    match vr with 
      | Actual r -> if List.mem r acc || r = "$fp" || r = "$ra" then acc else r::acc
      | Stacked i -> acc
    ) alloc [] in

  let code' = if mx > 0 then Instr(Unop("$sp", Addi (-4*mx), "$sp")) @@ tr_seq fdef.code
  else tr_seq fdef.code in
    
  {
    name = Aimp.(fdef.name);
    params = List.length Aimp.(fdef.params);
    locals = mx;
    code = code';
    registers = pushed;
  }

let tr_prog prog = {
    globals = Aimp.(prog.globals);
    functions = List.map tr_fdef Aimp.(prog.functions);
  }
