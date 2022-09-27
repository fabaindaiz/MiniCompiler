(** Lib **)
open Printf
open Asm
open Ast


(* data structure to save defined functions *)
type funenv = (string * int) list
(* data structure to save functions names *)
type nameenv = (string * string) list

(* data structure to attach registers to variables *)
type reg_env = (string * arg) list

let empty_regenv : reg_env = []

(* Obtiene el offset de la variable actual*)
let get_offset (env : reg_env) : int =
  1 + (List.length env)

(* extiende el ambiente con una variable del usuario *)
let extend_regenv (x : string) (env : reg_env) : (reg_env * int) =
  let reg_offset = (get_offset env) in
  ((x, RegOffset (RSP, reg_offset)) :: env, reg_offset)

let extend_regenv_reg (x : string * arg) (env : reg_env) : (reg_env) =
  let id, register = x in
  ((id, register) :: env)


(* gensym for string labels *)
let gensym =
  let a_counter = ref 0 in
  (fun basename ->
    a_counter := !a_counter + 1;
    sprintf "%s_%d" basename !a_counter);;


let error_tag = 1L
let err_not_number = 1L
let err_not_boolean = 2L

(* apply register test *)
let error_asm (error : int64) (ret : reg) (reg : reg) (num : int) (tag : int) : instruction list = 
  let test_label = sprintf "test%d_%d" num tag in
    [ ITest(Reg reg, Const error_tag) ; IJnz(test_label) ] @ (* Testea que el registro cumpla la condición *)
    [ IMov(Reg RSI, Reg ret) ; IMov(Reg RDI, Const error) ] @ (* Si no la cumple, prepara el error *)
    [ ICall("error") ; ILabel(test_label) ] (* Si la cumple, salta al label *)

(* !0x...0 & 0x1 = 0x1 *)
let error_not_number (reg : reg) (num : int) (tag : int) : instruction list = 
  [ IMov(Reg R11, Reg reg) ; IXor(Reg R11, Const error_tag) ] @ (error_asm err_not_number RAX R11 num tag)

(*  0x...1 & 0x1 = 0x1 *)
let error_not_boolean (reg : reg) (num : int) (tag : int) : instruction list = 
  (error_asm err_not_boolean reg reg num tag)


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
let callee_start (name : string) : instruction list =
  [ ILabel(name) ; IPush(Reg RBP) ; IMov(Reg RBP, Reg RSP) ]
  

(* return for callee *)
let callee_end : (instruction list) =
  [ IMov(Reg RSP, Reg RBP) ; IPop(Reg RBP) ; IRet ]

let callee_instrs (name : string) (instrs : instruction list) : instruction list =
  (callee_start name) @ instrs @ callee_end


let ccall_reg (num : int) : (instruction list) =
  match num with
  | 1 -> [ IMov(Reg RAX, Reg RDI) ]
  | 2 -> [ IMov(Reg RAX, Reg RSI) ]
  | 3 -> [ IMov(Reg RAX, Reg RDX) ]
  | 4 -> [ IMov(Reg RAX, Reg RCX) ]
  | 5 -> [ IMov(Reg RAX, Reg R8) ]
  | 6 -> [ IMov(Reg RAX, Reg R9) ]
  | _ -> [ IMov(Reg RAX, RegOffset(RSP, 0)) ] (* TODO verificar el offset que debe ir *)

let ctype_error (ctype : ctype) (num : int) (tag : int) : instruction list =
  match ctype with
  | CAny -> []
  | CInt -> (error_not_number RAX num tag)
  | CBool -> (error_not_boolean RAX num tag)

(* c calls type verification *)
let ccall_list (type_list : ctype list) (tag : int) : instruction list =
  let _ = callee_gensym true in
  List.fold_left (fun res i -> res @ 
    let num = (callee_gensym false) in
    (ccall_reg num) @ (ctype_error i num tag)) [] type_list

let ccall_ret (type_ret : ctype) (tag : int) : instruction list =
  (ctype_error type_ret 0 tag)

let callee_defsys (call_name : string) (fun_name : string) (type_list : ctype list) (type_ret : ctype) (tag : int) : instruction list =
  [ ILabel(call_name) ] @ (ccall_list type_list tag) @ [ ICall(fun_name) ] @
  (ccall_ret type_ret tag) @ [ IRet ]


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

(* get enviroment for args list *)  
let env_from_args (args : string list) : reg_env =
  let rec env_arg_help (l : string list) (count : int) : reg_env = 
    match l with
    | [] -> empty_regenv
    | id::tail ->
      (match count with
      | 1 -> extend_regenv_reg (id, (Reg RDI)) (env_arg_help tail (count+1))
      | 2 -> extend_regenv_reg (id, (Reg RSI)) (env_arg_help tail (count+1))
      | 3 -> extend_regenv_reg (id, (Reg RDX)) (env_arg_help tail (count+1))
      | 4 -> extend_regenv_reg (id, (Reg RCX)) (env_arg_help tail (count+1))
      | 5 -> extend_regenv_reg (id, (Reg R8)) (env_arg_help tail (count+1))
      | 6 -> extend_regenv_reg (id, (Reg R9)) (env_arg_help tail (count+1))
      | _ -> extend_regenv_reg (id, (RegOffset (RSP, count-6))) (env_arg_help tail (count+1)) )
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
let caller_val (target : string) (args : instruction list) (num : int) (stack_size : int): instruction list =
  [ ISub(Reg RSP, rsp_offset (stack_size*8)) ] @ caller_save @ args @ [ ICall(target) ] @ (* pass the arguments and call the function *)
  (if num >= 7 then [ IAdd(Reg RSP, rsp_offset (num - 6)) ] else []) @ caller_restore

let caller_instrs (target : string) (instrs_list : instruction list list) (stack_size : int): instruction list =
  let instrs = (List.rev (caller_args instrs_list)) in (* arguments for the call *)
  let args_num = List.length instrs_list in (* number of arguments *)
    (caller_val target instrs args_num stack_size)
