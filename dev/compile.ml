open Ast
open Asm

let rec compile_expr (e : expr) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, Const n) ] 
  | _ -> failwith "TO BE DONE!"

let compile e : string =
  let instrs = compile_expr e in
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])
