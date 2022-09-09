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
let bool_mask = 0x8000000000000000L
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
  | Prim1 (op, e) -> let insts =
    (match op with
    | Add1 -> [IAdd ((Reg RAX), Const 2L)]
    | Sub1 -> [IAdd ((Reg RAX), Const (-2L))]
    | Not -> 
      [ IMov (Reg RBX, Const bool_mask) ] @
      [ IXor (Reg RAX, Reg RBX) ]) in 
      (compile_expr e env var_count) @ insts
  | Id s -> [ IMov (Reg RAX, Reg (List.assoc s env))] (* mueve valor desde la pila a RAX *)
  | Let (id, e, body) -> 
      (compile_expr e env var_count) @ (* se extrae valor de e y queda en RAX *)
      [IMov (Reg (RSP var_count), Reg RAX)] @ (* se pasa el valor de RAX a la direccion RSP disponible *)
      (compile_expr body (extend_regenv id (RSP var_count) env) (var_count + 1)) (* se compila body con nuevo env *)
  | Prim2 (op, e1, e2) -> 
    let prelude =
      (compile_expr e2 env var_count) @ (* set value of e2 in RAX *)
      [IMov (Reg (RSP var_count), Reg RAX)] @ (* moves value to stack *)
      (compile_expr e1 env (var_count + 1))(* solve e1 with var_count offset *) in

    let jump_label = gensym "label" in
    let condition (inst : instruction list) : (instruction list) = (* to generate comparative functions*)
      [ ICmp (Reg RAX, Reg (RSP var_count))] @ (* compares values (left operand e1 in RAX) *)
      [ IMov (Reg RAX, Const val_true) ] @ inst @ (* preemptively sets false and check condition *)
      [ IMov (Reg RAX, Const val_false) ; ILabel(jump_label) ] in (* if true, overrides RAX *)

    let lazy_eval (inst : instruction list) (icond_jmp : instruction) (value : int64): instruction list =
      (compile_expr e2 env var_count) @ (* evalua un operando *) [IMov (Reg RBX, Const value)] @ (* no puedo comparar con imm64?? *)
      [ICmp (Reg RAX, Reg RBX)] @ [icond_jmp] @ (* compara value con resultado y si se cumple termina *)
      [IMov (Reg (RSP var_count), Reg RAX)] @ (* si no continua normal *)
      (compile_expr e1 env (var_count + 1)) @ inst @ [ILabel jump_label] in
    (match op with
    | Add -> prelude @ [IAdd (Reg RAX, Reg (RSP var_count))] (* operates value saved in stack with prev value and sets it in RAX*)
    | Sub -> prelude @ [ISub (Reg RAX, Reg (RSP var_count))]
    | Mul -> prelude @ [IMul (Reg RAX, Reg (RSP var_count))] @ [ ISar (Reg RAX, Const 1L) ]
    | Div -> prelude @ [IDiv (Reg (RSP var_count))] @ [ ISal (Reg RAX, Const 1L) ]
    | And -> lazy_eval [IAnd (Reg RAX, Reg (RSP var_count))] (IJe jump_label) val_false
    | Or -> lazy_eval [IOr (Reg RAX, Reg (RSP var_count))] (IJe jump_label) val_true
    | Lte -> prelude @ condition ([IJle jump_label])
    | Gte -> prelude @ condition ([IJge jump_label])
    | Lt -> prelude @ condition ([IJl jump_label])
    | Gt -> prelude @ condition ([IJg jump_label])
    | Eq -> prelude @ condition ([IJe jump_label])
    | Neq -> prelude @ condition ([IJne jump_label]))
  | If (c, t, e) ->
      let else_label = gensym "if_label" in
      let done_label = gensym "done" in
      (compile_expr c env var_count) @ [IMov (Reg RBX, Const val_true)] @ [ ICmp(Reg RAX, Reg RBX) ; IJne(else_label) ] @
      (compile_expr t env var_count) @ [ IJmp(done_label) ; ILabel(else_label) ] @
      (compile_expr e env var_count) @ [ ILabel(done_label) ]

let compile e : string =
  (* variables parten colocandose desde RSP - 8*1 *)
  let instrs = compile_expr e empty_regenv 1 in 
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])