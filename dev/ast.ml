(** AST **)

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
  
(* Pretty printing *)
let rec string_of_expr(e : expr) : string = 
  match e with
  | Num n -> Int64.to_string n
  | Var s -> s
  (* TO BE COMPLETED *)
  | _ -> failwith "TODO" 
