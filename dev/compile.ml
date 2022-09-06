open Ast
open Asm

(* data structure to attach registers to variables *)
type reg_env = (string * reg) list
let empty_regenv : reg_env = []
let extend_regenv : string -> reg -> reg_env -> reg_env =
  fun x v env -> (x, v) :: env

(* constants *)
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let int_tag = 0L
let bool_tag = 1L
let val_true = Int64.add Int64.min_int bool_tag (* 10..01*)
let val_false = bool_tag (* 00..01*)
let rec compile_expr (e : expr) (env : reg_env) (var_count : int) : instruction list =
  match e with 
  | Num n -> 
    if n > max_int || n < min_int then
      failwith ("Integer overflow: " ^ (Int64.to_string n))
    else
      [ IMov (Reg RAX, Const (Int64.shift_left n 1)) ] 
  | Bool b -> let val_rep = (if b then val_true else val_false) in
      [ IMov (Reg RAX, Const (val_rep)) ] 
  | Prim1 (op, e) -> let inst =
    (match op with
    | Add1 -> IAdd ((Reg RAX), Const 2L)
    | Sub1 -> IAdd ((Reg RAX), Const (-2L))) in 
      (compile_expr e env var_count) @ [inst]
  | Id s -> [ IMov (Reg RAX, Reg (List.assoc s env))] (* mueve valor desde la pila a RAX *)
  | Let (id, e, body) -> 
      (compile_expr e env var_count) @ (* se extrae valor de e y queda en RAX *)
      [IMov (Reg (RSP var_count), Reg RAX)] @ (* se pasa el valor de RAX a la direccion RSP disponible *)
      (compile_expr body (extend_regenv id (RSP var_count) env) (var_count + 1)) (* se compila body con nuevo env *)
  | Prim2 (op, e1, e2) -> 
        (compile_expr e1 env var_count) @ (* set value of 1 expr in RAX *)
        [IMov (Reg (RSP var_count), Reg RAX)] @ (* moves value to stack *)
        (compile_expr e2 env (var_count + 1)) @ (* solve e2 with var_count offset *)
      (match op with
      | Add -> [IAdd (Reg RAX, Reg (RSP var_count))] (* operates value saved in stack with prev value and sets it in RAX*)
      | And -> [IAnd (Reg RAX, Reg (RSP var_count))]
      | Lte -> let labeltrue = gensym("ltetrue") in let labeldone = gensym("ltedone") in
        [Cmp (Reg (RSP var_count), Reg RAX)] @
        [Jle labeltrue] @ (* if true jumps to label true *)
        [IMov (Reg RAX, Const val_false)] @ (* if not moves val false to RAX*)
        [Jmp labeldone] @ (* ends *)
        [Label labeltrue; IMov (Reg RAX, Const val_true)] @(* true branch *)
        [Label labeldone])
         (* operates value saved in stack with prev value and sets it in RAX*)
  | _ -> failwith "TO BE DONE!"

let compile e : string =
  (* variables parten colocandose desde RSP - 8*1 *)
  let instrs = compile_expr e empty_regenv 1 in 
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])