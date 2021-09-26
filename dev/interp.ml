(** Interpreter **)
open Ast

(** Values **)
type value = NumV of int64 | BoolV of bool

(* Pretty printing *)
let string_of_val(v : value) : string =
  match v with
  | NumV n -> Int64.to_string n
  | BoolV b -> if b then "true" else "false"

(* Lifting functions on OCaml primitive types to operate on language values *)
let liftIII : (int64 -> int64 -> int64) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)
    | _ -> failwith "runtime type error"

let liftBBB : (bool -> bool -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | BoolV b1, BoolV b2 -> BoolV (op b1 b2)
    | _ -> failwith "runtime type error"    

let liftIIB : (int64 -> int64 -> bool) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> BoolV (op n1 n2)
    | _ -> failwith "runtime type error"

(* Environment *)
type env = (string * value) list
let empty_env : env = []
let extend_env : string -> value -> env -> env =
  fun x v env -> (x, v) :: env

(* interpreter *)
let rec interp expr env =
  match expr with
  | Id x -> List.assoc x env
  | Num n -> NumV n
  | Bool b -> BoolV b
  | Prim1 (op, e) -> 
    (match op with
    | Add1 -> liftIII ( Int64.add )
    | Sub1 -> liftIII ( Int64.sub )) (interp e env) (NumV 1L)
  | Prim2 (op, e1, e2) -> 
    (match op with
    | Add -> liftIII ( Int64.add ) 
    | And -> liftBBB ( && ) 
    | Lte -> liftIIB ( <= )) (interp e1 env) (interp e2 env)
  | Let (x, e , b) -> interp b (extend_env x (interp e env) env)
  | If (e1, e2, e3) -> 
    (match (interp e1 env) with
    | BoolV b -> if b then interp e2 env else interp e3 env
    | _ -> failwith "runtime type error")
