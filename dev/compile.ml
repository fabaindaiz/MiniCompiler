(** Compiler **)
open Printf
open Asm
open Ast
open Lib

  
(* constants *)
let min_int = Int64.div Int64.min_int 2L
let max_int = Int64.div Int64.max_int 2L
let bool_tag = 1L
let tuple_tag = 5L
let closure_tag = 7L
let bool_mask = 0x8000000000000000L
let val_true = Int64.add Int64.min_int bool_tag (* 10..01*)
let val_false = bool_tag (* 00..01*)

(* compila y verifica tuple y index y luego verifica que index este dentro del rango de tupla
   termina con valor de indice en RAX y registro con puntero a tupla (untagged) en "utuple_reg".
   Retorna instrucciones, direccion tupla (tagged) y registro que contiene direccion (untagged) *) 
let check_index_in_bounds (compile_expr) (tuple : tag eexpr) (index : tag eexpr) (utuple_reg : reg) (tag : tag) (env : renv) (fenv : fenv) (nenv : nenv) : instruction list * arg =
  let (env', reg_offset) = extend_renv (sprintf "temp_%d_1" tag) env in
  let tuple_arg = RegOffset (RBP, reg_offset) in

  (compile_expr tuple env' fenv nenv) @ (type_error_check ETuple RAX tag 3) @ (* compila tupla *)
  [ IMov (tuple_arg, Reg RAX) ] @ (* guarda la tupla *)

  (compile_expr index env' fenv nenv) @ (type_error_check ENum RAX tag 4) @ (* compila índice *)
  [ IMov (Reg utuple_reg, tuple_arg); ISub (Reg R10, Const tuple_tag) ] @ (* save untagged pointer in passed reg*)
  [ IMov (Reg R11, RegOffset (utuple_reg, 0)) ] @ (* guarda largo de tupla en R11*)

  (* make sure the index is non-negative and is within the size of the tuple *)
  [ ISar (Reg RAX, Const 1L) ] @ (error_tuple_bad_index (Reg RAX) tuple_arg (Reg R11) tag)
  , tuple_arg


(* compila primitivas unarias *)
let compile_prim1 (compile_expr) (op : prim1) (e : tag eexpr) (tag : tag) (env : renv) (fenv : fenv) (nenv : nenv) : instruction list =
  let insts =
    (match op with
    | Add1 -> (type_error_check ENum RAX tag 1) @ [ IAdd (Reg RAX, Const 2L) ]
    | Sub1 -> (type_error_check ENum RAX tag 1) @ [ IAdd (Reg RAX, Const (-2L)) ]
    | Not -> (type_error_check EBool RAX tag 1) @ [ IMov (Reg R11, Const bool_mask) ; IXor (Reg RAX, Reg R11) ] ) in 
    (compile_expr e env fenv nenv) @ insts


(* compila primitivas binarias *)
let compile_prim2 (compile_expr) (op : prim2) (e1 : tag eexpr) (e2 : tag eexpr) (tag : tag) (env : renv) (fenv : fenv) (nenv : nenv) : instruction list =
  let (env', reg_offset) = extend_renv (sprintf "temp_%d_1" tag) env in
  let jump_label = sprintf "label_%d" tag in (* generates unique label *)

  (* prelude to no-lazy binary primitives *)
  let normal_eval (inst : instruction list) (etype : etype) : instruction list =
    (compile_expr e2 env' fenv nenv) @ (type_error_check etype RAX tag 2) @ (* set value of e2 in RAX *)
    
    [IMov (RegOffset (RBP, reg_offset), Reg RAX)] @ (* moves value to stack *)
    (compile_expr e1 env' fenv nenv) @ (type_error_check etype RAX tag 1) @ inst in

  (* if after computing one operand the result is equal to value, doesn't compute second value and mantains result 
  else compute inst as normal *)
  let lazy_eval (inst : instruction list) (etype : etype) (value : int64) : instruction list =
    (compile_expr e1 env' fenv nenv) @ (type_error_check etype RAX tag 1) @ (* evalua un operando *)
    
    (* compara value con resultado y si es igual termina *)
    [ IMov (Reg R11, Const value) ; ICmp (Reg RAX, Reg R11) ; IJe jump_label ] @
    
    [IMov (RegOffset (RBP, reg_offset), Reg RAX)] @ (* si no continua normal *)
    (compile_expr e2 env' fenv nenv) @ (type_error_check etype RAX tag 2) @ inst @ [ILabel jump_label] in

  (* to generate comparative operations*)
  let cond_eval (inst : instruction list) : instruction list =
    (* compares values, preemptively sets false and check condition *)
    [ ICmp (Reg RAX, RegOffset (RBP, reg_offset)) ; IMov (Reg RAX, Const val_true) ] @ inst @
    [ IMov (Reg RAX, Const val_false) ; ILabel (jump_label) ] in (* if true, overrides RAX *)

  let tuple_eval : instruction list =
    (* make sure the index is within the size of the tuple *)
    let instrs, _ = (check_index_in_bounds compile_expr e1 e2 R10 tag env' fenv nenv) in  
    instrs @ [ IAdd (Reg RAX, Const 1L) ; IMul (Reg RAX, Const 8L) ] @ (* get pointer to nth word *)
    [ IMov (Reg RAX, HeapOffset(R10, RAX)) ] in (* treat R11 as a pointer, and get its nth word *)
  
  let reg_offset = get_offset env in
    (match op with
    (* operates value saved in stack with prev value and sets it in RAX*)
    | Add -> (normal_eval [IAdd (Reg RAX, RegOffset (RBP, reg_offset))] ENum)
    | Sub -> (normal_eval [ISub (Reg RAX, RegOffset (RBP, reg_offset))] ENum)
    | Mul -> (normal_eval ([IMul (Reg RAX, RegOffset (RBP, reg_offset)) ; ISar (Reg RAX, Const 1L)]) ENum)
    | Div -> (normal_eval ([IDiv (RegOffset (RBP, reg_offset))] @ [ ISal (Reg RAX, Const 1L)]) ENum)
    | And -> (lazy_eval [IAnd (Reg RAX, RegOffset (RBP, reg_offset))] EBool val_false)
    | Or -> (lazy_eval [IOr (Reg RAX, RegOffset (RBP, reg_offset))] EBool val_true)
    | Lt -> (normal_eval (cond_eval [IJl (jump_label)]) ENum)
    | Gt -> (normal_eval (cond_eval [IJg (jump_label)]) ENum)
    | Lte -> (normal_eval (cond_eval [IJle (jump_label)]) ENum)
    | Gte -> (normal_eval (cond_eval [IJge (jump_label)]) ENum)
    | Eq -> (normal_eval (cond_eval [IJe (jump_label)]) ENum)
    | Neq -> (normal_eval (cond_eval [IJne (jump_label)]) ENum)
    | Get -> tuple_eval )


let rec compile_expr (e : tag eexpr) (env : renv) (fenv : fenv) (nenv : nenv): instruction list =
  match e with 
  | ENum (n, _) -> 
    if n > max_int || n < min_int then
      failwith (sprintf "Integer overflow: %Ld" n)
    else [ IMov (Reg RAX, Const (Int64.shift_left n 1)) ] 
  | EBool (b, _) ->
    let val_rep = (if b then val_true else val_false) in
    [ IMov (Reg RAX, Const (val_rep)) ]
  | EId (s, _) ->
    (match List.assoc_opt s env with
    | Some arg -> [ IMov (Reg RAX, arg) ] (* mueve valor desde la pila a RAX *)
    | None -> failwith (sprintf "unbound variable %s in renv" s) )
  | EPrim1 (op, e, tag) -> 
    (compile_prim1 compile_expr op e tag env fenv nenv)
  | EPrim2 (op, e1, e2, tag) -> 
    (compile_prim2 compile_expr op e1 e2 tag env fenv nenv)
  | ELet (id, e, body, _) -> 
    let (env', reg_offset) = extend_renv id env in
    (compile_expr e env fenv nenv) @ (* se extrae valor de e y queda en RAX *)
    [ IMov (RegOffset (RBP, reg_offset), Reg RAX) ] @ (* se pasa el valor de RAX a la direccion RBP disponible *)
    (compile_expr body env' fenv nenv) (* se compila body con nuevo env *)
  | EIf (c, t, e, tag) -> 
    let else_label = sprintf "if_false_%d" tag in
    let done_label = sprintf "done_%d" tag in
    (compile_expr c env fenv nenv) @ (type_error_check EBool RAX tag 1) @
    [ IMov (Reg R11, Const val_true) ; ICmp (Reg RAX, Reg R11) ; IJne(else_label) ] @
    (compile_expr t env fenv nenv) @ [ IJmp (done_label) ; ILabel (else_label) ] @
    (compile_expr e env fenv nenv) @ [ ILabel (done_label) ]
  | EApp (f, p, _) -> 
    let args_f = (List.assoc_opt f fenv) in
    let name_f = (List.assoc_opt f nenv) in
    (match args_f with
    | Some n -> if (n == List.length p) then
      let elist = (compile_elist compile_expr p env fenv nenv) in
      (match name_f with
      | Some f' -> (caller_instrs [(ICall f')] elist) (* defsys *)
      | None -> (caller_instrs [(ICall f)] elist) ) (* deffun *)
      else failwith(sprintf "Arity mismatch: %s expected %d arguments but got %d" f n (List.length p))
    | None -> failwith(sprintf "undefined funtion: %s" f) )
  | ETuple(elist, tag) ->
    let (env', reg_offset) = extend_renv (sprintf "temp_%d" tag) env in
    let init_R15 = RegOffset (RBP, reg_offset) in
    let tuple_size = List.length elist in
    (* compile_tuple : ejecuta instruciones para expr list y mueve los elementos a el heap *)
    let rec compile_tuple (exprs : tag eexpr list) (count : int) : instruction list  =
      match exprs with
      | [] -> []
      | e :: tail ->
        (compile_expr e env' fenv nenv) @ (* Compila el valor *)
        [IMov (Reg R11, init_R15)] @
        [IMov (RegOffset (R11, count), Reg RAX)] @ (* Lo pone en el heap *)
        (compile_tuple tail (count + 1)) (* Hace lo mismo con el resto *) in
      [ IMov (init_R15, Reg R15) ] @ (* moves heap pointer to stack *)
      [ IAdd (Reg R15, Const (Int64.of_int ((tuple_size + 1) * 8)))] @ (* offsetea puntero de heap*)
      [ IMov (Reg R11, init_R15) ] @
      [ IMovq (RegOffset (R11, 0), Const (Int64.of_int tuple_size))] @ (* Ubica el numero de elementos *)
      (compile_tuple elist 1) @ (* Compila la tupla *)
      [ IMov (Reg RAX, init_R15) ] @ (* coloca direccion de tupla en RAX*)
      [ IAdd (Reg RAX, Const tuple_tag) ] (* taggea valor *)
  | ESet (t, n, v, tag) -> 
    let (env', reg_value) = extend_renv (sprintf "temp_%d_3" tag) env in
    (compile_expr v env' fenv nenv) @ [ IMov (RegOffset (RBP, reg_value), Reg RAX) ] @
    
    let instrs, tuple_arg = (check_index_in_bounds compile_expr t n R10 tag env' fenv nenv) in
      instrs @ (* make sure the index is within the size of the tuple *)

    (* index in rax, pointer to tuple in R10 *)
    [ IAdd (Reg RAX, Const 1L) ; IMul (Reg RAX, Const 8L) ] @ (* get pointer to nth word *)
    [ IMov (Reg R11, RegOffset (RBP, reg_value)) ; IMov (HeapOffset (R10, RAX), Reg R11) ] @ (* move v to nth word *)
    [ IMov (Reg RAX, tuple_arg) ] (* returns tuple *)
  
  | ELambda (params, body, tag) ->
    let fun_name = sprintf "lambda_%d" tag in
    let end_name = sprintf "end_%d" tag in
    
    (* get free vars *)
    let free_vars = (get_free_vars e []) in
    let env' = (env_from_args params) @ env in
    
    (* update closure arguments *)
    let clos_pack = (closure_pack free_vars R15 env') in
    let clos_unpack = (closure_unpack free_vars) in
    let env' = (closure_env free_vars env') in

    let args_num = (Int64.of_int (List.length params)) in
    let heap_offset= (Int64.of_int (((List.length free_vars) + 3) * 8)) in
    
    (* compile function *)
    let callee_lambda = clos_unpack @ [ ICom ("lambda body") ] @ (compile_expr body env' fenv nenv) in
    [ IJmp (end_name) ] @ (callee_instrs fun_name callee_lambda (num_expr e)) @ [ ILabel (end_name) ] @
    
    [ ICom ("closure information") ] @
    [ IMovq (RegOffset(R15, 0), Const args_num) ] @ (* set arg size at pos 0 *)
    [ IMovq (RegOffset(R15, 1), Any fun_name) ] @ clos_pack @ 
    [ ICom ("closure value") ] @
    [ IMov (Reg RAX, Reg R15) ; IAdd (Reg RAX, Const closure_tag) ] @ (* create lambda tuple *)
    [ IAdd (Reg R15, Const heap_offset) ] (* set func label at pos 1 *)
    
  | ELamApp (fe, ael, tag) ->
    let arity = (Int64.of_int (List.length ael)) in
    let (env', reg_offset) = extend_renv (sprintf "clos_%d_1" tag) env in

    let elist = (compile_elist compile_expr ael env' fenv nenv) in
    
    (* closure error checks instructions and call second val of func tuple *)
    let caller_lambda = [ IMov (Reg RAX, RegOffset(RBP, reg_offset)) ] @ (type_error_check EClosure RAX tag 1) @
    [ ISub (Reg RAX, Const closure_tag) ; IMov (Reg R11, RegOffset (RAX, 0)) ] @
    (error_arity_mismatch (Reg R11) (Const arity) tag 2) @ [ ICallArg (RegOffset (RAX, 1)) ] in

    (compile_expr fe env fenv nenv) @ [ IMov (RegOffset (RBP, reg_offset), Reg RAX) ] @
    [ ICom (sprintf "call lambda" ) ] @ (caller_instrs caller_lambda elist)

  | ELetRec (recs, body, _) -> failwith ("TODO rec")


(* Caso 1 : Compilación normal *)

(* Compila cada ambiente y función 
  Utilizando esta compilación una función solo se pueden llamar a si misma o otra funciones definidas antes *)

(* compile a enviroment & function *)
let compile_function (func : tag efundef) (fenv : fenv) (nenv : nenv) : (instruction list * fenv * nenv) =
  match func with
  | EDefFun (fun_name, arg_list, e, _) ->
    let fenv' = (fun_name, List.length arg_list) :: fenv in (* definir esto antes permite funciones recursivas *)
    let env = (env_from_args arg_list) in
    let instrs = (compile_expr e env fenv' nenv) in

    (*
    let overwrited = (get_overwrite_reg instrs) in
    (* if ((List.length overwrited) > 0) then (printf "%d" (List.length overwrited)) else (); *)
    let env' = (function_env overwrited env) in
    let instrs = (compile_expr e env' fenv' nenv) in
    *)

    (callee_instrs fun_name instrs (num_expr e + (List.length arg_list)), fenv', nenv)
  | EDefSys (fun_name, type_list, type_ret, tag) ->
    let call_name = fun_name ^ "_sys" in
    let fenv' = (fun_name, List.length type_list) :: fenv in
    let nenv' = (fun_name, call_name) :: nenv in (* almacena el nombre interno para cada defsys *)
      (callee_defsys call_name fun_name type_list type_ret tag, fenv', nenv')

(* compile several functions *)
let compile_functions (flist : tag efundef list) : (instruction list * fenv * nenv) =
  let rec compile_functions_help (flist : tag efundef list) (fenv : fenv) (nenv : nenv) : (instruction list * fenv * nenv) =
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
let compile_enviroment (func : tag efundef) (fenv : fenv) (nenv : nenv) : (fenv * nenv) =
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
let compile_function (func : tag efundef) (fenv : fenv) (nenv : nenv) : instruction list =
  match func with
  | EDefFun (fun_name, arg_list, e, _) ->
    let env = (env_from_args arg_list) in
    let instrs = (compile_expr e env fenv nenv) in

    (*
    (* Descomentar esto *)

    let overwrited = (get_overwrite_reg instrs) in
    (* if ((List.length overwrited) > 0) then (printf "%d" (List.length overwrited)) else (); *)
    let env' = (function_env overwrited env) in
    let instrs = (compile_expr e env' fenv' nenv) in
    *)

    (callee_instrs fun_name instrs (num_expr e))
  | EDefSys (fun_name, type_list, type_ret, tag) ->
    let call_name = fun_name ^ "_sys" in
    (callee_defsys call_name fun_name type_list type_ret tag)

(* compile several functions *)
let compile_functions_alt (flist : tag efundef list) : (instruction list * fenv * nenv) =
  let rec compile_enviroments_help (flist : tag efundef list) (fenv : fenv) (nenv : nenv) : (fenv * nenv) =
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
  
  let heap_prelude = [ ICom("heap prelude") ] @
  [ IMov(Reg R15, Reg RDI) ; IAdd(Reg R15, Const 23L) ; IMov(Reg R11, Const 0xfffffffffffffff8L); IAnd (Reg R15, Reg R11) ] in
  
  (* compile main expresion *)
  let instrs = (compile_expr tagged_expr empty_env fenv nenv) in
  let einstrs = (callee_instrs "our_code_starts_here" (heap_prelude @ instrs) (num_expr tagged_expr)) in

  (* variables internas *)
  let defsys_list, _ = List.split nenv in
  let extern_list = [ "error" ; "error2" ] @ defsys_list in
  let extern_string = (List.fold_left (fun res i -> res ^ sprintf "  extern %s\n" i) "" extern_list) in

  (* compile program *)
  (prelude extern_string) ^ (pp_instrs finstrs) ^ (pp_instrs einstrs)
