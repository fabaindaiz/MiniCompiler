open Ast
open Asm

type reg_env = (string * reg) list
let empty_regenv : reg_env = []
let extend_regenv : string -> reg -> reg_env -> reg_env =
  fun x v env -> (x, v) :: env

let rec compile_expr (e : expr) (env : reg_env) (var_count : int) : instruction list =
  match e with 
  | Num n -> [ IMov (Reg RAX, Const n) ] 
  | Prim1 (op, e) -> let inst =
    (match op with
    | Add1 -> Inc (Reg RAX)
    | Sub1 -> Dec (Reg RAX)) in 
      (compile_expr e env var_count) @ [inst]
  | Id s -> [ IMov (Reg RAX, Reg (List.assoc s env))] (* mueve valor desde la pila a RAX *)
  | Let (id, e, body) -> 
      (compile_expr e env var_count) @ (* se extrae valor de e y queda en RAX *)
      [IMov (Reg (RSP var_count), Reg RAX)] @ (* se pasa el valor de RAX a la direccion RSP disponible *)
      (compile_expr body (extend_regenv id (RSP var_count) env) (var_count + 1)) (* se compila body con nuevo env *)
  | Prim2 (op, e1, e2) -> 
    let iop =
      (match op with
      | Add -> IAdd
      | _ -> failwith "TO BE DONE!") in
        (compile_expr e1 env var_count) @ (* set value of 1 expr in RAX *)
        [IMov (Reg (RSP var_count), Reg RAX)] @ (* moves value to stack *)
        (compile_expr e2 env (var_count + 1)) @ (* solve e2 with var_count offset *)
        [IPrim2 (iop, Reg RAX, Reg (RSP var_count))] (* adds value saved in stack with prev value and sets it in RAX*)
  | _ -> failwith "TO BE DONE!"

let compile e : string =
  (* variables parten colocandose desde RSP - 8*1 *)
  let instrs = compile_expr e empty_regenv 1 in 
  let prelude ="
section .text
global our_code_starts_here
our_code_starts_here:" in
  prelude ^ pp_instrs (instrs @ [ IRet ])