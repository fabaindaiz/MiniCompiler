(* Fun *)
open Asm
open Ast


let bool_tag = 1L
let tuple_tag = 5L
let closure_tag = 7L


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
  | (_, RegOffset(RBP, _))::tail -> -1 + get_offset tail (* cuenta solo registros que apuntan a la pila *)
  | _::tail -> get_offset tail

(* extiende el ambiente con una variable del usuario *)
let extend_renv (x : string) (env : renv) : (renv * int) =
  let reg_offset = (get_offset env) in
  ((x, RegOffset (RBP, reg_offset)) :: env, reg_offset)

let extend_renv_reg (x : string * arg) (env : renv) : (renv) =
  let id, reg = x in
  ((id, reg) :: env)

(* 
let rec print_env (env : renv) : string =
  match env with
  | (id, reg) :: rest -> (sprintf "(%s, %s) " id (pp_arg reg)) ^ print_env rest
  | _ -> ""  *)


(* RAX : actual value *)
let type_error_check (t : etype): instruction list =
  match t with
  | EAny -> []
  | ENum -> [ ICall("test_num") ]
  | EBool -> [ ICall("test_bool") ]
  | ETuple -> [ ICall("test_tuple") ]
  | EClosure -> [ ICall("test_closure") ]

(* RAX : actual size, 11 : size limit, reg : original tuple *)
let error_tuple_bad_index (reg : arg) : instruction list =
  [ IPush(Reg RDI) ; IPush(Reg RSI) ; IMov(Reg RDI, reg) ; ICall("test_tuple_bad_index"); IPop(Reg RSI) ; IPop(Reg RDI) ]

(* R11 : actual arity, reg : expected arity *)
let error_arity_mismatch (reg : arg) : instruction list =
  [ IPush(Reg RDI) ; IPush(Reg RSI) ; IMov(Reg RDI, reg) ; ICall("test_arity_mismatch"); IPop(Reg RSI) ; IPop(Reg RDI) ]


let rsp_mask = 0xfffffff0

(* make RSP value multiple of 16, rounding up *)
let rsp_offset (num : int) : arg =
  Const (Int64.of_int ((num + 8) land rsp_mask))


(* prelude for callee *)
let callee_start (name : string) (num : int): instruction list =
  [ ILabel(name) ] @ [ ICom("prologue") ] @ [ IPush(Reg RBP) ; IMov(Reg RBP, Reg RSP) ] @
  [ ISub(Reg RSP, (rsp_offset (num  * 8))) ; ICom("function body") ]

(* return for callee *)
let callee_end : (instruction list) =
  [ ICom ("epilogue") ; IMov(Reg RSP, Reg RBP) ; IPop(Reg RBP) ; IRet ]

(* callee instructions *)
let callee_instrs (name : string) (instrs : instruction list) (num : int) : instruction list =
  (callee_start name num) @ instrs @ callee_end


let ctype_error (instr : instruction list) (ctype : ctype) : instruction list =
  match ctype with
  | CAny -> []
  | CInt -> instr @ (type_error_check ENum)
  | CBool -> instr @ (type_error_check EBool)
  | CTuple _ -> instr @ (type_error_check ETuple)

(* c calls args type verification *)
let ccall_args (args : ctype list) : (instruction list) =
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
      (ctype_error instr ctype) @ (ccall_args_help tail (count+1)) in
    (ccall_args_help args 1)

(* defsys callee instructions *)
let callee_defsys (call_name : string) (fun_name : string) (type_list : ctype list) (type_ret : ctype) : instruction list =
  let instr = (ccall_args type_list) @ [ ICall(fun_name) ] @ (ctype_error [] type_ret) in
  (callee_instrs call_name instr 0)


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
  (env_arg_help args 1)

  
(* generate an instruction for each argument *)
let caller_args (instrs : instruction list list) : instruction list =
  let new_instrs = List.rev instrs in
  let arg_len = List.length instrs in
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
    instr @ save @ (caller_args_help tail (count-1)) in
  [ ICom("load args") ] @ (caller_args_help new_instrs arg_len)

let caller_save : instruction list =
  [ ICom("prologue") ] @ [ IPush(Reg R9) ; IPush(Reg R8) ; IPush(Reg RCX) ; IPush(Reg RDX) ; IPush(Reg RSI) ; IPush(Reg RDI) ]

let caller_restore (num : int) : instruction list = 
  [ ICom("epilogue") ] @ (if num >= 7 then [ IAdd(Reg RSP, Const (Int64.of_int ((num - 6) * 8))) ] else []) @
  [ IPop(Reg RDI) ; IPop(Reg RSI) ; IPop(Reg RDX) ; IPop(Reg RCX) ; IPop(Reg R8) ; IPop(Reg R9) ] @ [ ICom("end call") ]

(* generate instruction for call *)
let caller_instrs (calli : instruction list) (args : instruction list list) : instruction list =
  caller_save @ (caller_args args) @ [ ICom("call function") ] @ calli @ (caller_restore (List.length args))


let request_memory (bytes_req : int64) : instruction list =
  [ IMov(Reg R11, Const bytes_req) ; ICall("request_memory") ]

let request_memory_defsys : instruction list =
  let instrs = caller_save @
    [ IMov(Reg RDI, Reg R15) ; IMov(Reg RSI, Reg R11) ] @
    [ IMov(Reg RDX, Reg RBP) ; IMov(Reg RCX, Reg RSP)] @ (* supongo que cur_frame y cur_rsp son rbp y rsp *)
    [ ICall("try_gc") ; IMov(Reg R15, Reg RAX)] @ (* Si cambia alloc_ptr se actualiza valor de R15 *)
    (caller_restore 0) in
  (callee_instrs "request_memory" instrs 0)


let err_not_number = 1L
let err_not_boolean = 2L
let err_not_tuple = 3L
let err_not_closure = 4L

let error_asm (error : int64) (label : string) : instruction list = 
  [ IJz(label) ; IMov(Reg RSI, Reg RAX) ; IMov(Reg RDI, Const error) ] @ (* Si no la cumple, prepara el error *)
  [ ICall("error") ; ILabel(label) ] (* Si la cumple, salta al label *)

let type_error_defsys : instruction list =
  let instrs = (* 0x...0 & 0x1 = 0x0 *)
    [ ITest(Reg RAX, Const 1L) ] @ (error_asm err_not_number "label_num") in
  (callee_instrs "test_num" instrs 0) @
  let instrs = (*  (0x...1 - 0x1) & 0x111 = 0x0 *)
    [ IMov(Reg R11, Reg RAX) ; ISub(Reg R11, Const bool_tag) ] @
    [ ITest(Reg R11, Const 7L) ] @ (error_asm err_not_boolean "label_bool") in
  (callee_instrs "test_bool" instrs 0) @
  let instrs = (* (0x...11 - 0x11) & 0x111 = 0x0 *)
    [ IMov(Reg R11, Reg RAX) ; ISub(Reg R11, Const tuple_tag) ] @
    [ ITest(Reg R11, Const 7L) ] @ (error_asm err_not_tuple "label_tuple") in
  (callee_instrs "test_tuple" instrs 0) @
  let instrs = (* (0x...111 - 0x111) & 0x111 = 0x0 *)
    [ IMov(Reg R11, Reg RAX) ; ISub(Reg R11, Const closure_tag) ] @
    [ ITest(Reg R11, Const 7L) ] @ (error_asm err_not_closure "label_closure") in
  (callee_instrs "test_closure" instrs 0)


let err_tuple_bad_index = 1L
let err_arity_mismatch = 2L

let error2_asm (error : int64) (label : string) : instruction list = 
  [ IMov(Reg RDX, Reg RDI) ; IMov(Reg RSI, Reg RAX) ; IMov(Reg RDI, Const error) ] @ (* Si no la cumple, prepara el error *)
  [ ICall("error2") ; ILabel(label) ] (* Si la cumple, salta al label *)

let type_error2_defsys : instruction list =
  let instrs =
    [ ICmp (Reg RAX, Const 0L) ; IJl("label_bad_index") ] @
    [ ICmp (Reg RAX, Reg R11) ; IJge("label_bad_index") ] @
    [ IJmp("label_index") ; ILabel("label_bad_index") ; ISal (Reg RAX, Const 1L) ] @
    (error2_asm err_tuple_bad_index "label_index") in
  (callee_instrs "test_tuple_bad_index" instrs 0) @
  let instrs =
    [ ICmp (Reg R11, Reg RDI) ; IJz("label_arity") ; IMov (Reg RAX, Reg R11) ] @
    (error2_asm err_arity_mismatch "label_arity") in
  (callee_instrs "test_arity_mismatch" instrs 0)
