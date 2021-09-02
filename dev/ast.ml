(** AST **)
open Printf

(* primitive operators *)
type prim1 = Add1 | Sub1
type prim2 = Add | And | Lte 

(* Algebraic datatype for expressions *)
type expr = 
  | Num of int64 
  | Bool of bool
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Id of string
  | Let of string * expr * expr
  | If of expr * expr * expr
  
(* Pretty printing - used by testing framework *)
let rec string_of_expr(e : expr) : string = 
  match e with
  | Num n -> Int64.to_string n
  | Bool b -> if b then "true" else "false"
  | Id s -> s
  | Prim1 (op, e) -> sprintf "(%s %s)" 
    (match op with
    | Add1 -> "add1"
    | Sub1 -> "sub1") (string_of_expr e)
  | Prim2 (op, e1, e2) -> sprintf "(%s %s %s)" 
    (match op with 
    | Add -> "+"
    | And -> "and"
    | Lte -> "<=") (string_of_expr e1) (string_of_expr e2)
  | Let (x, e1, e2) -> sprintf "(let (%s %s) %s)" x (string_of_expr e1) (string_of_expr e2) 
  | If (e1, e2, e3) -> sprintf "(if %s %s %s)" (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
