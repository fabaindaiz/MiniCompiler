(** Compiler **)
open Printf
open Asm
open Ast
open Lib


(* calculate an aprox number of used argument *)
(* TODO Verificar que esto esta correcto *)
let rec num_expr (expr : tag eexpr) : int =
  match expr with
  | ENum (_, _) -> 1
  | EBool (_, _) -> 1
  | ETuple (elist, _) -> num_expr_list elist
  | EId (_, _) -> 1
  | EPrim1 (_, e1, _) -> 1 + (num_expr e1)
  | EPrim2 (_, e1, e2, _) -> 1 + (max (num_expr e1) (num_expr e2))
  | ELet (_, e1, e2, _) -> 1 + (max (num_expr e1) (num_expr e2))
  | EIf (c, e1, e2, _) -> 1 + (max (num_expr c) (max (num_expr e1) (num_expr e2)))
  | ESet (c, e1, e2, _) -> 1 + (max (num_expr c) (max (num_expr e1) (num_expr e2)))
  | EApp (_, elist, _) -> num_expr_list elist
  and num_expr_list (elist: tag eexpr list) : int =
    match elist with
    | [] -> 0
    | e1::tail -> (max (num_expr e1) (num_expr_list tail))

(* constants *)
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let int_tag = 0L
let bool_tag = 1L
let bool_mask = 0x8000000000000000L
let val_true = Int64.add Int64.min_int bool_tag (* 10..01*)
let val_false = bool_tag (* 00..01*)


(* compila primitivas unarias *)
let compile_prim1 (compile_expr) (op : prim1) (e : tag eexpr) (tag : tag) (env : reg_env) (fenv : funenv) (nenv : nameenv) : instruction list =
  let insts =
    (match op with
    | Add1 -> (error_not_number RAX 1 tag) @ [IAdd (Reg RAX, Const 2L)]
    | Sub1 -> (error_not_number RAX 1 tag) @ [IAdd (Reg RAX, Const (-2L))]
    | Not -> (error_not_boolean RAX 1 tag) @
      [ IMov (Reg R11, Const bool_mask) ; IXor (Reg RAX, Reg R11) ] ) in 
      (compile_expr e env fenv nenv) @ insts


(* compila primitivas binarias *)
let compile_prim2 (compile_expr) (op : prim2) (e1 : tag eexpr) (e2 : tag eexpr) (tag : tag) (env : reg_env) (fenv : funenv) (nenv : nameenv) : instruction list =
  let (env', reg_offset) = extend_regenv (sprintf "temp_%d" tag) env in
  let jump_label = sprintf "label_%d" tag in (* generates unique label *)

  (* prelude to no-lazy binary primitives *)
  let normal_eval (inst : instruction list) (test) : instruction list =
    (compile_expr e2 env fenv nenv) @ (test RAX 2 tag) @ (* set value of e2 in RAX *)
    
    [IMov (RegOffset (RBP, reg_offset), Reg RAX)] @ (* moves value to stack *)
    (compile_expr e1 env' fenv nenv) @ (test RAX 1 tag) @ inst in

  (* if after computing one operand the result is equal to value, doesn't compute second value and mantains result 
  else compute inst as normal *)
  let lazy_eval (inst : instruction list) (test) (value : int64) : instruction list =
    (compile_expr e1 env fenv nenv) @ (test RAX 1 tag) @ (* evalua un operando *)
    
    (* compara value con resultado y si es igual termina *)
    [ IMov (Reg R11, Const value) ; ICmp (Reg RAX, Reg R11) ; IJe jump_label ] @
    
    [IMov (RegOffset (RBP, reg_offset), Reg RAX)] @ (* si no continua normal *)
    (compile_expr e2 env' fenv nenv) @ (test RAX 2 tag) @ inst @ [ILabel jump_label] in

  (* to generate comparative operations*)
  let cond_eval (inst : instruction list) : (instruction list) =
    (* compares values, preemptively sets false and check condition *)
    [ ICmp (Reg RAX, RegOffset (RBP, reg_offset)) ; IMov (Reg RAX, Const val_true) ] @ inst @
    [ IMov (Reg RAX, Const val_false) ; ILabel(jump_label) ] in (* if true, overrides RAX *)
  
  let reg_offset = get_offset env in
    (match op with
    (* operates value saved in stack with prev value and sets it in RAX*)
    | Add -> normal_eval [IAdd (Reg RAX, RegOffset (RBP, reg_offset))] error_not_number
    | Sub -> normal_eval [ISub (Reg RAX, RegOffset (RBP, reg_offset))] error_not_number
    | Mul -> normal_eval ([IMul (Reg RAX, RegOffset (RBP, reg_offset))] @ [ ISar (Reg RAX, Const 1L) ]) error_not_number
    | Div -> normal_eval ([IDiv (RegOffset (RBP, reg_offset))] @ [ ISal (Reg RAX, Const 1L) ]) error_not_number
    | And -> lazy_eval [IAnd (Reg RAX, RegOffset (RBP, reg_offset))] error_not_boolean val_false
    | Or -> lazy_eval [IOr (Reg RAX, RegOffset (RBP, reg_offset))] error_not_boolean val_true
    | Lt -> normal_eval (cond_eval [ IJl jump_label ]) error_not_number
    | Gt -> normal_eval (cond_eval [ IJg jump_label ]) error_not_number
    | Lte -> normal_eval (cond_eval [ IJle jump_label ]) error_not_number
    | Gte -> normal_eval (cond_eval [ IJge jump_label ]) error_not_number
    | Eq -> normal_eval (cond_eval [ IJe jump_label ]) error_not_number
    | Neq -> normal_eval (cond_eval [ IJne jump_label ]) error_not_number )


let rec compile_expr (e : tag eexpr) (env : reg_env) (fenv : funenv) (nenv : nameenv): instruction list =
  match e with 
  | ENum (n, _) -> 
    if n > max_int || n < min_int then
      failwith (sprintf "Integer overflow: %Ld" n)
    else [ IMov (Reg RAX, Const (Int64.shift_left n 1)) ] 
  | EBool (b, _) ->
    let val_rep = (if b then val_true else val_false) in
      [ IMov (Reg RAX, Const (val_rep)) ]
  | EId (s, _) -> begin match List.assoc_opt s env with
    | Some arg ->[ IMov (Reg RAX, arg)] (* mueve valor desde la pila a RAX *)
    | None -> failwith(sprintf "unbound variable %s in regenv" s)
    end
  | EPrim1 (op, e, tag) -> 
    (compile_prim1 compile_expr op e tag env fenv nenv)
  | EPrim2 (op, e1, e2, tag) -> 
    (compile_prim2 compile_expr op e1 e2 tag env fenv nenv)
  | ELet (id, e, body, _) -> 
    let (env', reg_offset) = extend_regenv id env in
      (compile_expr e env fenv nenv) @ (* se extrae valor de e y queda en RAX *)
      [ IMov (RegOffset (RBP, reg_offset), Reg RAX) ] @ (* se pasa el valor de RAX a la direccion RBP disponible *)
      (compile_expr body env' fenv nenv) (* se compila body con nuevo env *)
  | EIf (c, t, e, tag) -> 
    let else_label = sprintf "if_false_%d" tag in
    let done_label = sprintf "done_%d" tag in
      (compile_expr c env fenv nenv) @ (error_not_boolean RAX 1 tag) @
      [ IMov (Reg R11, Const val_true) ; ICmp(Reg RAX, Reg R11) ; IJne(else_label) ] @
      (compile_expr t env fenv nenv) @ [ IJmp(done_label) ; ILabel(else_label) ] @
      (compile_expr e env fenv nenv) @ [ ILabel(done_label) ]
  | EApp (f, p, _) -> 
    let args_f = (List.assoc_opt f fenv) in
    let name_f = (List.assoc_opt f nenv) in
    let compile_elist (exprs : tag eexpr list) : instruction list list =
      List.fold_left (fun res i -> res @ [ (compile_expr i env fenv nenv) ]) [] exprs in
      (match args_f with
        | Some n -> if (n == List.length p) then
          (match name_f with
            | Some f' -> (caller_instrs f' (compile_elist p)) (* defsys *)
            | None -> (caller_instrs f (compile_elist p)) ) (* deffun *)
          else failwith(sprintf "Arity mismatch: %s expected %d arguments but got %d" f n (List.length p))
        | None -> failwith(sprintf "undefined funtion: %s" f) )


(* Caso 1 : Compilación normal *)

(* Compila cada ambiente y función 
  Utilizando esta compilación una función solo se pueden llamar a si misma o otra funciones definidas antes *)

(* compile a enviroment & function *)
let compile_function (func : tag efundef) (fenv : funenv) (nenv : nameenv) : (instruction list * funenv * nameenv) =
  match func with
  | EDefFun (fun_name, arg_list, e, _) ->
    let fenv' = (fun_name, List.length arg_list) :: fenv in (* definir esto antes permite funciones recursivas *)
    let instrs = (compile_expr (e) (env_from_args arg_list) fenv' nenv) in
      (callee_instrs fun_name instrs (num_expr e + (List.length arg_list)), fenv', nenv)
  | EDefSys (fun_name, type_list, type_ret, tag) ->
    let call_name = fun_name ^ "_sys" in
    let fenv' = (fun_name, List.length type_list) :: fenv in
    let nenv' = (fun_name, call_name) :: nenv in (* almacena el nombre interno para cada defsys *)
      (callee_defsys call_name fun_name type_list type_ret tag, fenv', nenv')

(* compile several functions *)
let compile_functions (flist : tag efundef list) : (instruction list * funenv * nameenv) =
  let rec compile_functions_help (flist : tag efundef list) (fenv : funenv) (nenv : nameenv) : (instruction list * funenv * nameenv) =
    match flist with
    | [] -> [], [], []
    | flist_head::flist_tail -> 
      let instrs', fenv', nenv' = (compile_function flist_head fenv nenv) in
      let t_instrs, t_fenv, t_nenv = (compile_functions_help flist_tail fenv' nenv') in
        (instrs' @ t_instrs, fenv' @ t_fenv, nenv' @ t_nenv) in
  (compile_functions_help flist [] [])


(* Caso 2 : Compilación por pasos *)

(* Compila todos los ambiente y luego todas las funciones
  Utilizando esta compilación una función pueden llamar otras funciones definidas despues de esta *)

(* compile a enviroment *)
let compile_enviroment (func : tag efundef) (fenv : funenv) (nenv : nameenv) : (funenv * nameenv) =
  match func with
  | EDefFun (fun_name, arg_list, _, _) ->
    let fenv' = (fun_name, List.length arg_list) :: fenv in
      (fenv', nenv)
  | EDefSys (fun_name, type_list, _, _) ->
    let call_name = fun_name ^ "_sys" in
    let fenv' = (fun_name, List.length type_list) :: fenv in
    let nenv' = (fun_name, call_name) :: nenv in (* almacena el nombre interno para cada defsys *)
      (fenv', nenv')

(* compile a function *)
let compile_function (func : tag efundef) (fenv : funenv) (nenv : nameenv) : instruction list =
  match func with
  | EDefFun (fun_name, arg_list, e, _) ->
    let instrs = (compile_expr (e) (env_from_args arg_list) fenv nenv) in
      (callee_instrs fun_name instrs (num_expr e))
  | EDefSys (fun_name, type_list, type_ret, tag) ->
    let call_name = fun_name ^ "_sys" in
      (callee_defsys call_name fun_name type_list type_ret tag)

(* compile several functions *)
let compile_functions_alt (flist : tag efundef list) : (instruction list * funenv * nameenv) =
  let rec compile_enviroments_help (flist : tag efundef list) (fenv : funenv) (nenv : nameenv) : (funenv * nameenv) =
    match flist with
    | [] -> [], []
    | flist_head::flist_tail -> 
      let fenv', nenv' = (compile_enviroment flist_head fenv nenv) in
      let t_fenv, t_nenv = (compile_enviroments_help flist_tail fenv' nenv') in
        (fenv' @ t_fenv, nenv' @ t_nenv) in
  let fenv', nenv' = (compile_enviroments_help flist [] []) in
  let instrs = List.fold_left (fun res f -> res @ (compile_function f fenv' nenv')) [] flist in
    (instrs, fenv', nenv')


(* generate asm prelude *)
let prelude (defsys : string) = sprintf "
  section .text 
  global our_code_starts_here 
%s" defsys

(* compilation pipeline *)
let compile_prog (p : prog) : string =
  (* compile functions *)
  let tagged_flist, tagged_expr = (tag_program p) in
  let finstrs, fenv, nenv = (compile_functions_alt tagged_flist) in (* Usar compile_functions o compile_functions_alt *)
  
  (* compile main expresion *)
  let instrs = (compile_expr tagged_expr empty_regenv fenv nenv) in
  let einstrs = (callee_instrs "our_code_starts_here" instrs (num_expr tagged_expr)) in

  (* variables internas *)
  let defsys_list, _ = List.split nenv in
  let extern_list = [ "error" ] @ defsys_list in
  let defsys_string = (List.fold_left (fun res i -> res ^ sprintf "  extern %s\n" i) "" extern_list) in

  (* compile program *)
  (prelude defsys_string) ^ (pp_instrs finstrs) ^ (pp_instrs einstrs)
