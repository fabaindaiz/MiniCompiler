open Printf

(* registers *)
type reg = 
| RAX
| RSP of int (* integer is multiplied by 8 after *)

(* arguments for instructions *)
type arg =
| Const of int64
| Reg of reg

(* asm instructions *)
type instruction =
| IRet
| IMov of arg * arg
| Inc of arg
| Dec of arg
(* TO BE COMPLETED *)

let pp_reg reg : string =
  match reg with
  | RAX -> "RAX"
  | RSP n -> sprintf "[RSP - %d]" (n * 8)

let pp_arg arg : string =
  match arg with
  | Const n -> sprintf "%#Lx" n
  | Reg r -> pp_reg r

let pp_instr instr : string =
  match instr with
  | IRet -> "  ret" 
  | IMov (a1, a2) -> sprintf "  mov %s, %s" (pp_arg a1) (pp_arg a2)
  | Inc (a) -> sprintf "  inc %s" (pp_arg a)
  | Dec (a) -> sprintf "  dec %s" (pp_arg a)
  (* TO BE COMPLETED *)

let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs
