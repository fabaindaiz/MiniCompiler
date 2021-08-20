(** Lib of runtime structures (values, environments) **)

(** Values **)
type value = NumV of int

(* Pretty printing *)
let string_of_val(v : value) : string =
  match v with
  | NumV n -> string_of_int n

(* Lifting functions on int to operate on values *)
let liftNumV : (int -> int -> int) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)


(** Environments **)

(* Type alias
  ['a list] is the type of expressions returning a list of some type 'a
  Here environment as lists of pairs (i.e. association lists)
*)
type env = (string * value) list

let empty_env : env = []

let extend_env : string -> value -> env -> env =
  fun x v env -> (x, v) :: env
