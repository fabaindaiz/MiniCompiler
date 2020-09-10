

(* 
  <expr> ::= number
         |  (add1 <expr>)
         |  (sub1 <expr>)
*)

type expr = 
  | Num of int64
  | Add1 of expr
  | Sub1 of expr

open Fmt

let rec pp_exp fmt = function
  | Num n -> int64 fmt n
  | Add1 e -> pf fmt "(add1 %a)" pp_exp e
  | Sub1 e -> pf fmt "(sub1 %a)" pp_exp e