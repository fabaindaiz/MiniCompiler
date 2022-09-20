(** Lib **)
open Printf
open Asm


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
let error_asm (error : int64) (ret : reg) (reg : reg) (num : int) (tag : int) : (instruction list) = 
  let test_label = sprintf "test%d_%d" num tag in
    [ ITest(Reg reg, Const error_tag) ; IJnz(test_label) ] @ (* Testea que el registro cumpla la condición *)
    [ IMov(Reg RSI, Reg ret) ; IMov(Reg RDI, Const error) ] @ (* Si no la cumple, prepara el error *)
    [ ICall("error") ; ILabel(test_label) ] (* Si la cumple, salta al label *)

(* !0x...0 & 0x1 = 0x1 *)
let error_not_number (reg : reg) (num : int) (tag : int) : (instruction list) = 
  [ IMov(Reg R11, Reg reg) ; IXor(Reg R11, Const error_tag) ] @ (error_asm err_not_number RAX R11 num tag)

(*  0x...1 & 0x1 = 0x1 *)
let error_not_boolean (reg : reg) (num : int) (tag : int) : (instruction list) = 
  (error_asm err_not_boolean reg reg num tag)


let rsp_mask = 0xfffffffe

let rsp_offset (num : int) : arg =
  Const (Int64.of_int ((num + 1) land rsp_mask))


(* prelude for callee *)
let callee_start (name : string) (num : int) : (instruction list) =
  [ ILabel(name) ; IPush(Reg RSP) ; IMov(Reg RBP, Reg RSP) ] @
  [ ISub(Reg RSP, rsp_offset num) ]

(* return for callee *)
let callee_end : (instruction list) =
  [ IMov(Reg RSP, Reg RBP) ; IPop(Reg RSP) ; IRet ]

let callee_instrs (name : string) (instrs : instruction list) (num : int) : (instruction list) =
  (callee_start name num) @ instrs @ callee_end (* TODO num *)


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
let caller_match (num : int) : (instruction list) =
  match num with
  | 1 -> [ IMov(Reg RDI, Reg RAX) ]
  | 2 -> [ IMov(Reg RSI, Reg RAX) ]
  | 3 -> [ IMov(Reg RDX, Reg RAX) ]
  | 4 -> [ IMov(Reg RCX, Reg RAX) ]
  | 5 -> [ IMov(Reg R8, Reg RAX) ]
  | 6 -> [ IMov(Reg R9, Reg RAX) ]
  | _ -> [ IPush(Reg RAX) ]

(* generate an instruction for each argument *)
let caller_args (instrs : instruction list list) : (instruction list) =
  List.fold_left (fun res i -> res @ caller_match (caller_gensym false) @ i ) [] instrs

let caller_save =
  [ IPush(Reg R9) ; IPush(Reg R8) ; IPush(Reg RCX) ; IPush(Reg RDX) ; IPush(Reg RSI) ; IPush(Reg RDI) ]

let caller_restore = 
  [ IPop(Reg RDI) ; IPop(Reg RSI) ; IPop(Reg RDX) ; IPop(Reg RCX) ; IPop(Reg R8) ; IPop(Reg R9) ]

(* generate instruction for call *)
let caller_val (target : string) (args : instruction list) (num : int) : (instruction list) =
  caller_save @ args @ [ ICall(target) ] @ (* pass the arguments and call the function *)
  (if num >= 7 then [ IAdd(Reg RSP, rsp_offset (num - 6)) ] else []) @ caller_restore
  

let caller_instrs (target : string) (instrs_list : instruction list list) : (instruction list) =
  let instrs = (List.rev (caller_args instrs_list)) in (* arguments for the call *)
  let args_num = List.length instrs_list in (* number of arguments *)
  let _ = caller_gensym true in (* reset gensym function for other calls *)
    instrs @ (caller_val target instrs args_num)
