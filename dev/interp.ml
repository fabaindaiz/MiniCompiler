(** Interpreter **)

open Ast
open Lib

let rec interp : env -> exp -> value =
  fun env e ->
  match e with
  | Var x -> List.assoc x env (* Functions on list can be found in the [List] module and accessed with the [List.function] syntax *)
  | Num n -> NumV n
  | Plus  (e1, e2) -> liftNumV ( + ) (interp env e1) (interp env e2)
  | Times (e1, e2) -> liftNumV ( * ) (interp env e1) (interp env e2)
