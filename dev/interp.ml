(** Interpreter **)

open Ast
open Lib

let rec interp expr env =
  match expr with
  | Var x -> List.assoc x env (* Functions on list can be found in the [List] module and accessed with the [List.function] syntax *)
  | Num n -> NumV n
  | Bool b -> BoolV b
  | Prim1 (Add1, e) -> liftIII ( Int64.add ) (NumV 1L) (interp e env)
  | Prim2 (p, e1, e2) -> 
    (match p with
    | Add -> liftIII ( Int64.add ) 
    | And -> liftBBB ( && ) 
    | Lte -> liftIIB ( <= )) (interp e1 env) (interp e2 env)
  | Let (x, e , b) -> interp b (extend_env x (interp e env) env)
