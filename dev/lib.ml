(** Lib **)
open Printf
open Asm
open Ast


(* data structure to attach registers to variables *)
type regenv = (string * arg) list
(* data structure to save defined functions *)
type funenv = (string * int) list
(* data structure to save functions names *)
type nameenv = (string * string) list


type envs = (regenv * funenv * nameenv)




let empty_regenv : regenv = []

(* Obtiene el offset de la variable actual*)
let rec get_offset (env : regenv) : int =
  match env with
  | (_, RegOffset(_, _))::tail -> 1 + get_offset tail (* cuenta solo registros que apuntan a la pila *)
  | _::tail -> get_offset tail
  | _ -> 1

(* extiende el ambiente con una variable del usuario *)
let extend_regenv (x : string) (env : regenv) : (regenv * int) =
  let reg_offset = (get_offset env) in
  ((x, RegOffset (RBP, reg_offset)) :: env, reg_offset)

let extend_regenv_reg (x : string * arg) (env : regenv) : (regenv) =
  let id, register = x in
  ((id, register) :: env)


(* calculate an aprox number of used argument *)
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

    
(* gensym for string labels *)
let gensym =
  let a_counter = ref 0 in
  (fun basename ->
    a_counter := !a_counter + 1;
    sprintf "%s_%d" basename !a_counter);;


let err_not_number = 1L
let err_not_boolean = 2L
let err_not_tuple = 3L
let err_bad_index_low = 5L
let err_bad_index_high = 6L

(* apply register test *)
let error_asm (error : int64) (reg : reg) (label : string) : instruction list = 
  [ IMov(Reg RSI, Reg reg) ; IMov(Reg RDI, Const error) ] @ (* Si no la cumple, prepara el error *)
  [ ICall("error") ; ILabel(label) ] (* Si la cumple, salta al label *)

(* !0x...0 & 0x1 = 0x1 *)
let error_not_number (reg : reg) (num : int) (tag : int) : instruction list =
  let label = sprintf "test%d_%d" num tag in
    [ IMov(Reg R11, Reg reg) ; IXor(Reg R11, Const 1L) ] @
    [ ITest(Reg R11, Const 1L) ; IJnz(label) ] @
    (error_asm err_not_number RAX label)

(*  0x...1 & 0x1 = 0x1 *)
let error_not_boolean (reg : reg) (num : int) (tag : int) : instruction list =
  let label = sprintf "test_%d_%d" tag num in
    [ ITest(Reg reg, Const 1L) ; IJnz(label) ] @
    (error_asm err_not_boolean reg label)

(* 0x...1 & 0x11 = 0x11 *)
let error_not_tuple (reg : reg) (num : int) (tag : int) : instruction list =
  let label = sprintf "test_%d_%d" tag num in
    [ ITest(Reg reg, Const 3L) ; IJnz(label) ] @
    (error_asm err_not_tuple reg label) (* TODO *)


let error2_asm (error : int64) (reg1 : reg) (reg2 : reg) (label : string) : instruction list = 
  [ IMov(Reg RDX, Reg reg2) ; IMov(Reg RSI, Reg reg1) ; IMov(Reg RDI, Const error) ] @ (* Si no la cumple, prepara el error *)
  [ ICall("error2") ; ILabel(label) ] (* Si la cumple, salta al label *)

let error_bad_index_low (reg1 : reg) (reg2 : reg) (num : int) (tag : int) : instruction list =
  let label = sprintf "test_%d_%d" tag num in
    [ ICmp (Reg reg1, Const 0L) ; IJge(label) ; ISal (Reg reg1, Const 1L) ] @
    (error2_asm err_bad_index_low reg1 reg2 label)

let error_bad_index_high (reg1 : reg) (reg2 : reg) (lim : reg) (num : int) (tag : int) : instruction list =
  let label = sprintf "test_%d_%d" tag num in
    [ ICmp (Reg reg1, Reg lim) ; IJl(label) ; ISal (Reg reg1, Const 1L) ] @
    (error2_asm err_bad_index_high reg1 reg2 label)


let rsp_mask = 0xfffffff0

let rsp_offset (num : int) : arg =
  Const (Int64.of_int ((num + 8) land rsp_mask))


(* reseteable gensym for callee *)
let callee_gensym =
  let b_counter = ref 0 in
  (fun reset ->
    if reset then
      b_counter := 0
    else
      incr b_counter;
    !b_counter );;

(* prelude for callee *)
let callee_start (name : string) (num : int): instruction list =
  [ ILabel(name) ; IPush(Reg RBP) ; IMov(Reg RBP, Reg RSP); ISub(Reg RSP, (rsp_offset (num*8))) ]
  

(* return for callee *)
let callee_end : (instruction list) =
  [ IMov(Reg RSP, Reg RBP) ; IPop(Reg RBP) ; IRet ]

let callee_instrs (name : string) (instrs : instruction list) (num : int) : instruction list =
  (callee_start name num) @ instrs @ callee_end


let ccall_reg (num : int) : (instruction list) =
  match num with
  | 1 -> [ IMov(Reg RAX, Reg RDI) ]
  | 2 -> [ IMov(Reg RAX, Reg RSI) ]
  | 3 -> [ IMov(Reg RAX, Reg RDX) ]
  | 4 -> [ IMov(Reg RAX, Reg RCX) ]
  | 5 -> [ IMov(Reg RAX, Reg R8) ]
  | 6 -> [ IMov(Reg RAX, Reg R9) ]
  | _ -> [ IMov(Reg RAX, RegOffset(RBP, -num+5)) ] (* arg 7 está en rbp + 16 y de ahi subiendo (bajando¿) *)
        @ [IPush(Reg RAX)] (* como se movio la pila hay que volver a poner el valor *)

let ctype_error (ctype : ctype) (num : int) (tag : int) : instruction list =
  match ctype with
  | CAny -> []
  | CInt -> (error_not_number RAX num tag)
  | CBool -> (error_not_boolean RAX num tag)
  | CTuple _ -> failwith ("TODO")

(* c calls type verification *)
let ccall_list (type_list : ctype list) (tag : int) : instruction list =
  let _ = callee_gensym true in
  List.fold_left (fun res i -> res @ 
    let num = (callee_gensym false) in
    (ccall_reg num) @ (ctype_error i num tag)) [] type_list

let ccall_ret (type_ret : ctype) (tag : int) : instruction list =
  (ctype_error type_ret 0 tag) 

let callee_defsys (call_name : string) (fun_name : string) (type_list : ctype list) (type_ret : ctype) (tag : int) : instruction list =
  callee_start call_name 0 @ (ccall_list type_list tag) @ [ ICall(fun_name) ] @
  (ccall_ret type_ret tag) @ callee_end


(* reseteable gensym for caller *)
let caller_gensym =
  let c_counter = ref 0 in
  (fun reset ->
    if reset then
      c_counter := 0
    else
      incr c_counter;
    !c_counter );;

(* get instruction for argument *)
let caller_match (num : int) : instruction list =
  match num with
  | 1 -> [ IMov(Reg RDI, Reg RAX) ]
  | 2 -> [ IMov(Reg RSI, Reg RAX) ]
  | 3 -> [ IMov(Reg RDX, Reg RAX) ]
  | 4 -> [ IMov(Reg RCX, Reg RAX) ]
  | 5 -> [ IMov(Reg R8, Reg RAX) ]
  | 6 -> [ IMov(Reg R9, Reg RAX) ]
  | _ -> [ IPush(Reg RAX) ]

(* get enviroment from args list to compile function *)  
let env_from_args (args : string list) : regenv =
  let rec env_arg_help (l : string list) (count : int) : regenv = 
    match l with
    | [] -> empty_regenv
    | id::tail ->
      (match count with
      | 1 -> extend_regenv_reg (id, (Reg RDI))
      | 2 -> extend_regenv_reg (id, (Reg RSI)) 
      | 3 -> extend_regenv_reg (id, (Reg RDX)) 
      | 4 -> extend_regenv_reg (id, (Reg RCX)) 
      | 5 -> extend_regenv_reg (id, (Reg R8)) 
      | 6 -> extend_regenv_reg (id, (Reg R9)) 
      | _ -> extend_regenv_reg (id, (RegOffset (RBP, -count+5)))) (env_arg_help tail (count+1))
    in env_arg_help args 1 


(* generate an instruction for each argument *)
let caller_args (instrs : instruction list list) : instruction list =
  let _ = caller_gensym true in
  List.fold_left (fun res i -> res @ caller_match (caller_gensym false) @ List.rev i ) [] instrs

let caller_save =
  [ IPush(Reg R9) ; IPush(Reg R8) ; IPush(Reg RCX) ; IPush(Reg RDX) ; IPush(Reg RSI) ; IPush(Reg RDI) ]

let caller_restore = 
  [ IPop(Reg RDI) ; IPop(Reg RSI) ; IPop(Reg RDX) ; IPop(Reg RCX) ; IPop(Reg R8) ; IPop(Reg R9) ]

(* generate instruction for call *)
let caller_val (target : string) (args : instruction list) (num : int): instruction list =
  caller_save @ args @ [ ICall(target) ] @ (* pass the arguments and call the function *)
  (if num >= 7 then [ IAdd(Reg RSP, Const (Int64.of_int ((num - 6) * 8))) ] else []) @ caller_restore

let caller_instrs (target : string) (instrs_list : instruction list list): instruction list =
  let instrs = (List.rev (caller_args instrs_list)) in (* arguments for the call *)
  let args_num = List.length instrs_list in (* number of arguments *)
    (caller_val target instrs args_num)
