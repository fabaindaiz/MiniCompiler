(** ASM **)
open Printf


(* registers *)
type reg = 
| RAX (* the register where we place answers *)
| R11 (* temporary register *)
| R10 (* temporary register *)
| R9(* arg_6 *)
| R8 (* arg_5 *)
| RCX (* arg_4 *)
| RDX (* arg_3 *)
| RDI (* arg_2 *)
| RSI (* arg_1 *)
| RBP (* callee-saved register *)
| RSP (* the stack pointer, below which we can use memory *)
| R15 (* the heap pointer *)

(* arguments for instructions *)
type arg =
| Const of int64 (* explicit numeric constants *)
| Reg of reg (* any named and stack register *)
| Rel of string (* relative to the heap pointer *)
| RegOffset of reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)
| HeapOffset of reg * reg
| Any of string (* encapsulate any string as an arg *)

(* asm instructions *)
type instruction =
| IMov of arg * arg (* Move the value of the right-side arg into the left-arg *)
| IMovq of arg * arg
| ILea of arg * arg
| IAdd of arg * arg (* Increment the left-hand arg by the value of the right-hand arg *)
| ISub of arg * arg
| IMul of arg * arg
| IDiv of arg
| INot of arg * arg
| IAnd of arg * arg
| IOr of arg * arg
| IXor of arg * arg
| IShl of arg * arg
| IShr of arg * arg
| ISal of arg * arg
| ISar of arg * arg
| ICmp of arg * arg
| ITest of arg * arg
| IPush of arg
| IPop of arg
| ICom of string (* Comment *)
| ILabel of string
| ICall of string
| ICallArg of arg
| IJmp of string
| IJnz of string
| IJz of string
| IJl of string (* Less *)
| IJg of string (* Greater *)
| IJle of string (* LessEq *)
| IJge of string (* GreaterEq *)
| IJe of string (* Equal *)
| IJne of string (* Nequal *)
| IRet


(* registers to string *)
let pp_reg (reg : reg) : string =
  match reg with
  | RAX -> "RAX"
  | R11 -> "R11"
  | R10 -> "R10"
  | R9  -> "R9"
  | R8  -> "R8"
  | RCX -> "RCX"
  | RDX -> "RDX"
  | RSI -> "RSI"
  | RDI -> "RDI"
  | RBP -> "RBP"
  | RSP -> "RSP"
  | R15 -> "R15"

(* arguments for instruction to string *)
let pp_arg (arg : arg) : string =
  match arg with
  | Const n -> sprintf "%#Lx" n
  | Reg r -> pp_reg r
  | Rel s -> sprintf "[rel %s]" s
  | RegOffset (a1, a2) -> sprintf "[%s + %d]" (pp_reg a1) (8  * a2)
  | HeapOffset (a1, a2) -> sprintf "[%s + %s]" (pp_reg a1) (pp_reg a2)
  | Any s -> s

(* asm instruction to string *)
let pp_instr (instr : instruction) : string =
  match instr with
  | IMov (a1, a2) -> sprintf "  mov %s, %s" (pp_arg a1) (pp_arg a2)
  | IMovq (a1, a2) -> sprintf "  mov qword %s, %s" (pp_arg a1) (pp_arg a2)
  | ILea (a1, a2) -> sprintf "  lea %s, %s" (pp_arg a1) (pp_arg a2)
  | IAdd (a1, a2) -> sprintf "  add %s, %s" (pp_arg a1) (pp_arg a2)
  | ISub (a1, a2) -> sprintf "  sub %s, %s" (pp_arg a1) (pp_arg a2)
  | IMul (a1, a2) -> sprintf "  imul %s, %s" (pp_arg a1) (pp_arg a2)
  | IDiv (a1) -> sprintf "  mov RDX, 0\n  idiv qword %s" (pp_arg a1)
  | INot (a1, a2) -> sprintf "  not %s, %s" (pp_arg a1) (pp_arg a2)
  | IAnd (a1, a2) -> sprintf "  and %s, %s" (pp_arg a1) (pp_arg a2)
  | IOr (a1, a2) -> sprintf "  or %s, %s" (pp_arg a1) (pp_arg a2)
  | IXor (a1, a2) -> sprintf "  xor %s, %s" (pp_arg a1) (pp_arg a2)
  | IShl (a1, a2) -> sprintf "  shl %s, %s" (pp_arg a1) (pp_arg a2)
  | IShr (a1, a2) -> sprintf "  shr %s, %s" (pp_arg a1) (pp_arg a2)
  | ISal (a1, a2) -> sprintf "  sal %s, %s" (pp_arg a1) (pp_arg a2)
  | ISar (a1, a2) -> sprintf "  sar %s, %s" (pp_arg a1) (pp_arg a2)
  | ICmp (a1, a2) -> sprintf "  cmp %s, %s" (pp_arg a1) (pp_arg a2)
  | ITest (a1, a2) -> sprintf "  test %s, %s" (pp_arg a1) (pp_arg a2)
  | IPush (a1) -> sprintf "  push %s" (pp_arg a1)
  | IPop (a1) -> sprintf "  pop %s" (pp_arg a1)
  | ICom (s) -> sprintf "  ;; %s" s
  | ILabel (s) -> sprintf "%s:" s
  | ICall (s) -> sprintf "  call %s" s
  | ICallArg (a) -> sprintf "  call %s" (pp_arg a)
  | IJmp (s) -> sprintf "  jmp %s" s
  | IJnz (s) -> sprintf "  jnz %s" s
  | IJz (s) -> sprintf "  jz %s" s
  | IJl (s) -> sprintf "  jl %s" s
  | IJg (s) -> sprintf "  jg %s" s
  | IJle (s) -> sprintf "  jle %s" s
  | IJge (s) -> sprintf "  jge %s" s
  | IJe (s) -> sprintf "  je %s" s
  | IJne (s) -> sprintf "  jne %s" s
  | IRet -> sprintf "  ret"

(* asm instruction list to string *)
let pp_instrs (instrs : instruction list) : string =
  List.fold_left (fun res i -> res ^ "\n" ^ (pp_instr i)) "" instrs
