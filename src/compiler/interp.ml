open Fmt


(** Types *)

(* Base types: [string], [int] (31 or 63 bits), pairs ['a * 'b], lists ['a list] *)

(* Type alias *)
type id = string

(* Algebraic datatypes *)
type exp =
  | Var : id -> exp
  | Num : int -> exp
  | Plus : exp * exp -> exp
  | Times : exp * exp -> exp

let rec pp_exp : exp Fmt.t =
  fun fmt e ->
  match e with
  | Var s -> string fmt s
  | Num n -> int fmt n
  | Plus (e1, e2) -> pf fmt "(+ %a %a)" pp_exp e1 pp_exp e2
  | Times (e1, e2) -> pf fmt "(* %a %a)" pp_exp e1 pp_exp e2


(** Values *)

type value = NumV of int

let pp_value : value Fmt.t =
  fun fmt e ->
  match e with
  | NumV n -> int fmt n

let eq_value : value -> value -> bool = (=)

(* Lifting functions on int to values *)
let liftNumV : (int -> int -> int) -> value -> value -> value =
  fun op e1 e2 ->
  match e1, e2 with
  | NumV n1, NumV n2 -> NumV (op n1 n2)


(** Environments **)

(* Type constructors
 * ['a list] is the type of expressions returning a list of some type 'a
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
  | Plus (e1, e2) -> liftNumV ( + ) (interp env e1) (interp env e2)
  | Times (e1, e2) -> liftNumV ( * ) (interp env e1) (interp env e2)


(** Parsing *)
open Sexp

let rec parse : sexp -> exp =
  function
  | Atom s ->
    (* A frequent trouble: when nesting [match] the inner match must
     * be enclosed in [( ... )] or [begin ... end] *)
    begin match int_of_string_opt s with
      | Some n -> Num n
      | None -> Var s
    end
  | List [Atom "+" ; e1 ; e2 ] -> Plus (parse e1, parse e2)
  | List [Atom "*" ; e1 ; e2 ] -> Times (parse e1, parse e2)
  | e -> failwith "Not a valid exp: %a" pp_sexp e


let sexp_from_file : string -> sexp =
  fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> to_sexp s
  | Error msg ->
    Fmt.failwith "Unable to parse file %s: %s" filename msg




