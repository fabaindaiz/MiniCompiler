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
| IMov of arg * arg (* Move the value of the right-side arg into the left-arg *)
| IAdd of arg * arg (* Increment the left-hand arg by the value of the right-hand arg *)
| ISub of arg * arg
| IMul of arg * arg
| IDiv of arg * arg
| IAnd of arg * arg
| IOr of arg * arg
| IXor of arg * arg
| IShl of arg * arg
| IShr of arg * arg
| ISal of arg * arg
| ISar of arg * arg
| ICmp of arg * arg
| ILabel of string
| IJmp of string
| IJl of string (* Less *)
| IJg of string (* Greater *)
| IJle of string (* LessEq *)
| IJge of string (* GreaterEq *)
| IJe of string (* Equal *)
| IJne of string (* Nequal *)
| IRet

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
  | IMov (a1, a2) -> sprintf "  mov %s, %s" (pp_arg a1) (pp_arg a2)
  | IAdd (a1, a2) -> sprintf "  add %s, %s" (pp_arg a1) (pp_arg a2)
  | ISub (a1, a2) -> sprintf "  sub %s, %s" (pp_arg a1) (pp_arg a2)
  | IMul (a1, a2) -> sprintf "  imul %s, %s" (pp_arg a1) (pp_arg a2)
  | IDiv (a1, a2) -> sprintf "  div %s, %s" (pp_arg a1) (pp_arg a2)
  | IAnd (a1, a2) -> sprintf "  and %s, %s" (pp_arg a1) (pp_arg a2)
  | IOr (a1, a2) -> sprintf "  or %s, %s" (pp_arg a1) (pp_arg a2)
  | IXor (a1, a2) -> sprintf "  xor %s, %s" (pp_arg a1) (pp_arg a2)
  | IShl (a1, a2) -> sprintf "  shl %s, %s" (pp_arg a1) (pp_arg a2)
  | IShr (a1, a2) -> sprintf "  shr %s, %s" (pp_arg a1) (pp_arg a2)
  | ISal (a1, a2) -> sprintf "  sal %s, %s" (pp_arg a1) (pp_arg a2)
  | ISar (a1, a2) -> sprintf "  sar %s, %s" (pp_arg a1) (pp_arg a2)
  | ICmp (a1, a2) -> sprintf "  cmp %s, %s" (pp_arg a1) (pp_arg a2)
  | ILabel (s) -> sprintf "%s:" s
  | IJmp (s) -> sprintf "  jmp %s" s
  | IJl (s) -> sprintf "  jl %s" s
  | IJg (s) -> sprintf "  jg %s" s
  | IJle (s) -> sprintf "  jle %s" s
  | IJge (s) -> sprintf "  jge %s" s
  | IJe (s) -> sprintf "  je %s" s
  | IJne (s) -> sprintf "  jne %s" s
  | IRet -> sprintf "  ret"


let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs

(* gensym for something *)
let gensym =
  let counter = ref 0 in
  (fun basename ->
    counter := !counter + 1;
    sprintf "%s_%d" basename !counter);;