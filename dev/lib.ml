(** Lib **)
open Printf
open Set
open Asm
open Ast


(* data structure to attach registers to variables *)
type renv = (string * arg) list
(* data structure to save defined functions *)
type fenv = (string * int) list
(* data structure to save functions names *)
type nenv = (string * string) list


let empty_env : renv = []

(* Obtiene el offset de la variable actual*)
let rec get_offset (env : renv) : int =
  match env with
  | [] -> -1
  | (_, RegOffset(_, _))::tail -> -1 + get_offset tail (* cuenta solo registros que apuntan a la pila *)
  | _::tail -> get_offset tail

(* extiende el ambiente con una variable del usuario *)
let extend_renv (x : string) (env : renv) : (renv * int) =
  let reg_offset = (get_offset env) in
  ((x, RegOffset (RBP, reg_offset)) :: env, reg_offset)

let extend_renv_reg (x : string * arg) (env : renv) : (renv) =
  let id, reg = x in
  ((id, reg) :: env)


(* calculate an aprox number of used stack spaces *)
let rec num_expr (expr : tag eexpr) : int =
  match expr with
  | ENum (_, _) -> 0
  | EBool (_, _) -> 0
  | ETuple (elist, _) -> 1 + (num_expr_list elist)
  | EId (_, _) -> 0
  | EPrim1 (_, e1, _) -> (num_expr e1)
  | EPrim2 (_, e1, e2, _) -> 2 + (max (num_expr e1) (num_expr e2))
  | ELet (_, e1, e2, _) -> 1 + (max (num_expr e1) (num_expr e2))
  | EIf (c, e1, e2, _) -> (max (num_expr c) (max (num_expr e1) (num_expr e2)))
  | EApp (_, elist, _) -> (num_expr_list elist)
  | ESet (t, n, v, _) -> 2 + (max (num_expr t) (max (num_expr n) (num_expr v)))
  | ELambda (par, body, _) -> (num_expr_args (List.length par)) + (num_expr body)
  | ELamApp (fe, ael, _) -> 1 + max (num_expr_list ael) (num_expr fe)
  | ELetRec (recs, body, _) -> failwith ("TODO")
  and num_expr_args (arg_num : int) : int =
    if arg_num >= 7 then arg_num - 6 else 0
  and num_expr_list (elist: tag eexpr list) : int =
    match elist with
    | [] -> 0
    | e1::tail -> (max (num_expr e1) (num_expr_list tail))


let bool_tag = 1L
let tuple_tag = 5L
let closure_tag = 7L

let err_not_number = 1L
let err_not_boolean = 2L
let err_not_tuple = 3L
let err_not_closure = 4L

(* apply register test *)
let error_asm (error : int64) (reg : reg) (label : string) : instruction list = 
  [ IMov(Reg RDI, Const error) ; IMov(Reg RSI, Reg reg) ] @ (* Si no la cumple, prepara el error *)
  [ ICall("error") ; ILabel(label) ] (* Si la cumple, salta al label *)

let type_error_check (t : etype) (reg : reg) (tag : tag) (num : int): instruction list =
  let label = sprintf "test_%d_%d" tag num in
  [ ICom (label) ] @
  match t with
  | EAny -> []
  | ENum -> (* 0x...0 & 0x1 = 0x0 *)
    [ ITest(Reg reg, Const 1L) ; IJz(label) ] @
    (error_asm err_not_number reg label)
  | EBool -> (*  (0x...1 - 0x1) & 0x111 = 0x0 *)
    [ IMov(Reg R11, Reg reg) ; ISub(Reg R11, Const bool_tag) ] @
    [ ITest(Reg R11, Const 7L) ; IJz(label) ] @
    (error_asm err_not_boolean reg label)
  | ETuple -> (* (0x...11 - 0x11) & 0x111 = 0x0 *)
    [ IMov(Reg R11, Reg reg) ; ISub(Reg R11, Const tuple_tag) ] @
    [ ITest(Reg R11, Const 7L) ; IJz(label) ] @
    (error_asm err_not_tuple reg label)
  | EClosure -> (* (0x...111 - 0x111) & 0x111 = 0x0 *)
    [ IMov(Reg R11, Reg reg) ; ISub(Reg R11, Const closure_tag) ] @
    [ ITest(Reg R11, Const 7L) ; IJz(label) ] @
    (error_asm err_not_closure reg label)


let err_bad_index_low = 1L
let err_bad_index_high = 2L
let err_arity_mismatch = 3L

let error2_asm (error : int64) (reg1 : arg) (reg2 : arg) (label : string) : instruction list = 
  [ IMov(Reg RDI, Const error) ; IMov(Reg RSI, reg1) ; IMov(Reg RDX, reg2) ] @ (* Si no la cumple, prepara el error *)
  [ ICall("error2") ; ILabel(label) ] (* Si la cumple, salta al label *)

let error_tuple_bad_index (reg1 : arg) (reg2 : arg) (lim : arg) (tag : tag) : instruction list =
  let low_label = sprintf "test_%d_1" tag in
  let high_label = sprintf "test_%d_2" tag in
  [ ICom (low_label) ] @
  [ ICmp (reg1, Const 0L) ; IJge(low_label) ; ISal (reg1, Const 1L) ] @
  (error2_asm err_bad_index_low reg1 reg2 low_label) @
  [ ICom (high_label) ] @
  [ ICmp (reg1, lim) ; IJl(high_label) ; ISal (reg1, Const 1L) ] @
  (error2_asm err_bad_index_high reg1 reg2 high_label)

let error_arity_mismatch (reg1 : arg) (reg2 : arg) (tag : tag) (num : int) : instruction list =
  let label = sprintf "test_%d_%d" tag num in
  [ ICom (label) ] @ [ ICmp (reg1, reg2) ; IJz(label) ] @ (error2_asm err_arity_mismatch reg1 reg2 label)


let get_free_vars (e : tag eexpr) (l : string list) : string list =
  let rec get_free_vars_help (e : tag eexpr) (l : string list) : string list =
    match e with
    | ENum (_, _) -> []
    | EBool (_, _) -> []
    | ETuple (elist, _) -> (get_free_vars_help_list elist l)
    | EId (s, _) ->
      (match List.mem s l with
      | true -> []
      | false -> [s] )
    | EPrim1 (_, e1, _) -> (get_free_vars_help e1 l)
    | EPrim2 (_, e1, e2, _) -> (get_free_vars_help e1 l) @ (get_free_vars_help e2 l)
    | ELet (id, e, body, _) ->
      let l' = l @ [id] in
      (get_free_vars_help e l) @ (get_free_vars_help body l')
    | EIf (c, t, e, _) -> (get_free_vars_help c l) @ (get_free_vars_help t l) @ (get_free_vars_help e l)
    | EApp (_, p, _) -> (get_free_vars_help_list p l)
    | ESet (t, n, v, _) -> (get_free_vars_help t l) @ (get_free_vars_help n l) @ (get_free_vars_help v l)
    | ELambda (par, body, _) ->
      let l' = l @ (get_vars_list par l) in
      (get_free_vars_help body l')
    | ELamApp (fe, ael, _) -> (get_free_vars_help fe l) @ (get_free_vars_help_list ael l)
    | ELetRec (recs, body, _) -> failwith ("TODO")
    and get_free_vars_help_list (elist: tag eexpr list) (l : string list) : string list =
      match elist with
      | [] -> []
      | e::tail -> (get_free_vars_help e l) @ (get_free_vars_help_list tail l)
    and get_vars_list (slist: string list) (l : string list) : string list =
      match slist with
      | [] -> []
      | s::tail -> [s] @ (get_vars_list tail l) in
  let remove_duplicates (xs : string list) : string list = 
    let cons_uniq (xs : string list) (x : string) : string list = if List.mem x xs then xs else x :: xs in
    List.rev (List.fold_left cons_uniq [] xs) in
  (remove_duplicates (get_free_vars_help e l))


let rsp_mask = 0xfffffff0
let rsp_offset (num : int) : arg =
  Const (Int64.of_int ((num + 8) land rsp_mask))

(* prelude for callee *)
let callee_start (name : string) (num : int): instruction list =
  [ ILabel(name) ; IPush(Reg RBP) ; IMov(Reg RBP, Reg RSP); ISub(Reg RSP, (rsp_offset (num*8))) ] 

(* return for callee *)
let callee_end : (instruction list) =
  [ IMov(Reg RSP, Reg RBP) ; IPop(Reg RBP) ; IRet ]

(* callee instructions *)
let callee_instrs (name : string) (instrs : instruction list) (num : int) : instruction list =
  (callee_start name num) @ [ ICom (sprintf "start %s" name) ] @ instrs @
  [ ICom (sprintf "end %s" name) ] @ callee_end


let ctype_error (instr : instruction list) (ctype : ctype) (tag : tag) (num : int) : instruction list =
  match ctype with
  | CAny -> []
  | CInt -> instr @ (type_error_check ENum RAX tag num)
  | CBool -> instr @ (type_error_check EBool RAX tag num)
  | CTuple _ -> instr @ (type_error_check ETuple RAX tag num)

(* c calls args type verification *)
let ccall_args (args : ctype list) (tag : tag) : (instruction list) =
  let rec ccall_args_help (args : ctype list) (count : int) = 
    match args with
    | [] -> []
    | ctype::tail -> 
      let instr = (match count with
      | 1 -> [ IMov(Reg RAX, Reg RDI) ]
      | 2 -> [ IMov(Reg RAX, Reg RSI) ]
      | 3 -> [ IMov(Reg RAX, Reg RDX) ]
      | 4 -> [ IMov(Reg RAX, Reg RCX) ]
      | 5 -> [ IMov(Reg RAX, Reg R8) ]
      | 6 -> [ IMov(Reg RAX, Reg R9) ]
      | _ -> [ IMov(Reg RAX, RegOffset(RBP, count-5)) ; IPush(Reg RAX) ] ) in
      (* arg 7 está en rbp + 16 y de ahi subiendo, como se movio la pila hay que volver a poner el valor *)
      (ctype_error instr ctype tag count) @ (ccall_args_help tail (count+1)) in
    (ccall_args_help args 1)

(* defsys callee instructions *)
let callee_defsys (call_name : string) (fun_name : string) (type_list : ctype list) (type_ret : ctype) (tag : tag) : instruction list =
  let instr = (ccall_args type_list tag) @ [ ICall(fun_name) ] @ (ctype_error [] type_ret tag 0) in
  (callee_instrs call_name instr 0)
  

(* compile a tag expr list *)
let compile_elist (compile_expr) (exprs : tag eexpr list) (env : renv) (fenv : fenv) (nenv : nenv) : instruction list list =
  List.fold_left (fun res i -> res @ [ (compile_expr i env fenv nenv) ]) [] exprs

(* get enviroment from args list to compile function *)  
let env_from_args (args : string list) : renv =
  let rec env_arg_help (ids : string list) (count : int) : renv = 
    match ids with
    | [] -> empty_env
    | id::tail -> (match count with
      | 1 -> extend_renv_reg (id, (Reg RDI))
      | 2 -> extend_renv_reg (id, (Reg RSI)) 
      | 3 -> extend_renv_reg (id, (Reg RDX)) 
      | 4 -> extend_renv_reg (id, (Reg RCX)) 
      | 5 -> extend_renv_reg (id, (Reg R8)) 
      | 6 -> extend_renv_reg (id, (Reg R9)) 
      | _ -> extend_renv_reg (id, (RegOffset (RBP, count-5))) )
      (env_arg_help tail (count+1)) in
  env_arg_help args 1 

  
(* generate an instruction for each argument *)
let caller_args (instrs : instruction list list) : instruction list =
  let rec caller_args_help (l : instruction list list) (count : int) : instruction list =
    match l with
    | [] -> []
    | instr::tail ->
      let save = (match count with
      | 1 -> [ IMov(Reg RDI, Reg RAX) ]
      | 2 -> [ IMov(Reg RSI, Reg RAX) ]
      | 3 -> [ IMov(Reg RDX, Reg RAX) ]
      | 4 -> [ IMov(Reg RCX, Reg RAX) ]
      | 5 -> [ IMov(Reg R8, Reg RAX) ]
      | 6 -> [ IMov(Reg R9, Reg RAX) ]
      | _ -> [ IPush(Reg RAX) ] ) in
    instr @ save @ (caller_args_help tail (count+1)) in
  (caller_args_help instrs 1)

let caller_save : instruction list =
  [ IPush(Reg R9) ; IPush(Reg R8) ; IPush(Reg RCX) ; IPush(Reg RDX) ; IPush(Reg RSI) ; IPush(Reg RDI) ]

let caller_restore (arg_num : int) : instruction list = 
  (if arg_num >= 7 then [ IAdd(Reg RSP, Const (Int64.of_int ((arg_num - 6) * 8))) ] else []) @
  [ IPop(Reg RDI) ; IPop(Reg RSI) ; IPop(Reg RDX) ; IPop(Reg RCX) ; IPop(Reg R8) ; IPop(Reg R9) ]

(* generate instruction for call *)
let caller_instrs (calli : instruction list) (args : instruction list list) : instruction list =
  let arg_num = List.length args in
  caller_save @ (caller_args args) @ calli @ (caller_restore arg_num) (* pass the arguments and call the function *)
