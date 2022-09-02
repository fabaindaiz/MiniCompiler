open Ast
open Asm

let rec compile_expr (e : expr) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, Const n) ] 
  | Prim1 (op, e) -> let inst =
    (match op with
    | Add1 -> Inc (Reg RAX)
    | Sub1 -> Dec (Reg RAX)) in 
      (compile_expr e) @ [inst]
  | _ -> failwith "TO BE DONE!"

let compile e : string =
  let instrs = compile_expr e in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])
