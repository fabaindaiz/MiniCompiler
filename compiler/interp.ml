(** Types *)

(* Base types: [string], [int] (31 or 63 bits), pairs ['a * 'b], lists ['a list] *)

(* Type alias *)
type id = string

(* Algebraic datatypes *)
type exp =
  | Var of id
  | Num of int
  | Plus of exp * exp
  | Times of exp * exp


(** Values *)

type value = NumV of int

(* Lifting functions on int to values *)
let liftNumV : (int -> int -> int) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)



let rec  of_expr (e : Expr.expr) : exp =
  let module E = Expr in
  match e with 
  | E.Num n -> Num (Int64.to_int n) (* Beware ! Could fail ! *)  
  | E.Add1 e -> Plus (of_expr e, Num 1) 
  | E.Sub1 e -> Plus (of_expr e, Num ~-1)

(** Environments **)

(* Type constructors
   ['a list] is the type of expressions returning a list of some type 'a
  Here environment as lists of pairs (i.e. association lists)
*)
type env = (id * value) list

let empty_env : env = []

let extend_env : id -> value -> env -> env =
  fun x v env -> (x, v) :: env


(** Interpreter *)

let rec interp : env -> exp -> value =
  fun env e ->
  match e with
  | Var x ->
    (* Functions on list can be found in the [List] module and accessed with the [List.function] syntax *)
    List.assoc x env
  | Num n -> NumV n
  | Plus  (e1, e2) -> liftNumV ( + ) (interp env e1) (interp env e2)
  | Times (e1, e2) -> liftNumV ( * ) (interp env e1) (interp env e2)


(** Pretty printing **)

(* simple aliases to avoid confusion and use consistent naming *)
let pp_str = Fmt.string 
let pp_int = Fmt.int

(* printing values and expressions *)
let pp_value : value Fmt.t =
  fun fmt e -> 
    match e with 
    | NumV n -> pp_int fmt n

let rec pp_exp : exp Fmt.t =
  fun fmt e ->
    match e with
    | Var s -> pp_str fmt s
    | Num n -> pp_int fmt n
    | Plus (e1, e2) -> Fmt.pf fmt "(+ %a %a)" pp_exp e1 pp_exp e2
    | Times (e1, e2) -> Fmt.pf fmt "(* %a %a)" pp_exp e1 pp_exp e2

