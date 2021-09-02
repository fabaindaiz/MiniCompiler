(** AST **)
open Printf

(* primitive operators *)
type prim1 = Add1
type prim2 = Add | And | Lte 

(* Algebraic datatype for expressions *)
type expr = 
  | Num of int64 
  | Bool of bool
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Var of string
  | Let of string * expr * expr
  
(* Pretty printing - used by testing framework *)
let rec string_of_expr(e : expr) : string = 
  match e with
  | Num n -> Int64.to_string n
  | Bool b -> if b then "true" else "false"
  | Var s -> s
  | Prim1 (Add1, e) -> sprintf "(add1 %s)" (string_of_expr e)
  | Prim2 (op, e1, e2) -> sprintf "(%s %s %s)" 
    (match op with 
    | Add -> "+"
    | And -> "&&"
    | Lte -> "<=") (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
