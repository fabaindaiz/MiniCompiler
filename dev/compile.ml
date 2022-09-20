(** Compiler **)
open Printf
open Ast
open Asm

(* data structure to attach registers to variables *)
type reg_env = (string * reg) list

let empty_regenv : reg_env = []

(* Obtiene el offset de la variable actual*)
let get_offset (env : reg_env) : int =
  1 + (List.length env)

(* extiende el ambiente con una variable del usuario *)
let extend_regenv (x : string) (env : reg_env) : (reg_env * int) =
  let reg_offset = (get_offset env) in
  ((x, (RSP reg_offset)) :: env, reg_offset)


(* constants *)
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let int_tag = 0L
let bool_tag = 1L
let bool_mask = 0x8000000000000000L
let val_true = Int64.add Int64.min_int bool_tag (* 10..01*)
let val_false = bool_tag (* 00..01*)

let rec compile_expr (e : tag eexpr) (env : reg_env) : instruction list =
  match e with 
  | ENum (n, _) -> 
    if n > max_int || n < min_int then
      failwith ("Integer overflow: " ^ (Int64.to_string n))
    else
      [ IMov (Reg RAX, Const (Int64.shift_left n 1)) ] 
  | EBool (b, _) ->
    let val_rep = (if b then val_true else val_false) in
      [ IMov (Reg RAX, Const (val_rep)) ]
  | EId (s, _) -> [ IMov (Reg RAX, Reg (List.assoc s env))] (* mueve valor desde la pila a RAX *)
  | EPrim1 (op, e, _) -> let insts =
    (match op with
    | Add1 -> [IAdd ((Reg RAX), Const 2L)]
    | Sub1 -> [IAdd ((Reg RAX), Const (-2L))]
    | Not -> 
      [ IMov (Reg R11, Const bool_mask) ] @
      [ IXor (Reg RAX, Reg R11) ]) in 
      (compile_expr e env) @ insts
  | EPrim2 (op, e1, e2, tag) -> 
    let (env', reg_offset) = extend_regenv (sprintf "temp_%d" tag) env in
    let jump_label = sprintf "label_%d" tag in (* generates unique label *)

    (* prelude to no-lazy binary primitives *)
    let normal_eval (inst : instruction list) : instruction list =
      (compile_expr e2 env) @ (* set value of e2 in RAX *)
      
      [IMov (Reg (RSP reg_offset), Reg RAX)] @ (* moves value to stack *)
      (compile_expr e1 env') @ inst in

    (* if after computing one operand the result is equal to value, doesn't compute second value and mantains result 
    else compute inst as normal *)
    let lazy_eval (inst : instruction list) (value : int64) : instruction list =
      (compile_expr e2 env) @ (* evalua un operando *)
      
      (* lazy_eval es lo mismo que normal_eval con estas lineas extra y el label al final *)
      [IMov (Reg R11, Const value)] @ (* no puedo comparar con imm64?? no segun el profe *)
      [ICmp (Reg RAX, Reg R11)] @ [IJe jump_label] @ (* compara value con resultado y si es igual termina *)
      
      [IMov (Reg (RSP reg_offset), Reg RAX)] @ (* si no continua normal *)
      (compile_expr e1 env') @ inst @ [ILabel jump_label] in

    (* to generate comparative operations*)
    let cond_eval (inst : instruction list) : (instruction list) =
      [ ICmp (Reg RAX, Reg (RSP reg_offset))] @ (* compares values (left operand e1 in RAX) *)
      [ IMov (Reg RAX, Const val_true) ] @ inst @ (* preemptively sets false and check condition *)
      [ IMov (Reg RAX, Const val_false) ; ILabel(jump_label) ] in (* if true, overrides RAX *)
    
    let reg_offset = get_offset env in
    (match op with
    | Add -> normal_eval [IAdd (Reg RAX, Reg (RSP reg_offset))] (* operates value saved in stack with prev value and sets it in RAX*)
    | Sub -> normal_eval [ISub (Reg RAX, Reg (RSP reg_offset))]
    | Mul -> normal_eval [IMul (Reg RAX, Reg (RSP reg_offset))] @ [ ISar (Reg RAX, Const 1L) ]
    | Div -> normal_eval [IDiv (Reg (RSP reg_offset))] @ [ ISal (Reg RAX, Const 1L) ]
    | And -> lazy_eval [IAnd (Reg RAX, Reg (RSP reg_offset))] val_false
    | Or -> lazy_eval [IOr (Reg RAX, Reg (RSP reg_offset))] val_true
    | Lt -> normal_eval (cond_eval [ IJl jump_label ])
    | Gt -> normal_eval (cond_eval [ IJg jump_label ])
    | Lte -> normal_eval (cond_eval [ IJle jump_label ])
    | Gte -> normal_eval (cond_eval [ IJge jump_label ])
    | Eq -> normal_eval (cond_eval [ IJe jump_label ])
    | Neq -> normal_eval (cond_eval [ IJne jump_label ]) )
  | ELet (id, e, body, _) -> 
    let (env', reg_offset) = extend_regenv id env in
      (compile_expr e env) @ (* se extrae valor de e y queda en RAX *)
      [ IMov (Reg (RSP reg_offset), Reg RAX) ] @ (* se pasa el valor de RAX a la direccion RSP disponible *)
      (compile_expr body env') (* se compila body con nuevo env *)
  | EIf (c, t, e, tag) ->
    let else_label = sprintf "if_false_%d" tag in
      let done_label = sprintf "done_%d" tag in
      (compile_expr c env) @ [IMov (Reg R11, Const val_true)] @ [ ICmp(Reg RAX, Reg R11) ; IJne(else_label) ] @
      (compile_expr t env) @ [ IJmp(done_label) ; ILabel(else_label) ] @
      (compile_expr e env) @ [ ILabel(done_label) ]

(* compilation pipeline *)
let compile e : string =
  (* variables parten colocandose desde RSP - 8*1 *)
  let tagged = tag_expr e in
  let instrs = compile_expr tagged empty_regenv in 
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])