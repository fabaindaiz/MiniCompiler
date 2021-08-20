(** AST **)
open Printf

(* Algebraic datatype for expressions *)
type exp =
  | Var : string -> exp
  | Num : int -> exp
  | Plus : exp * exp -> exp
  | Times : exp * exp -> exp


(* Pretty printing *)
let rec string_of_exp(e : exp) : string = 
  match e with
  | Var x -> x
  | Num n -> string_of_int n
  | Plus (e1, e2) -> sprintf "(+ %s %s)" (string_of_exp e1) (string_of_exp e2)
  | Times (e1, e2) -> sprintf "(* %s %s)" (string_of_exp e1) (string_of_exp e2)

