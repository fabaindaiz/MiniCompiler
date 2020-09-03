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


(** Values *)

type value = NumV of int

(* Lifting functions on int to values *)
let liftNumV : (int -> int -> int) -> value -> value -> value =
  fun op e1 e2 ->
    match e1, e2 with
    | NumV n1, NumV n2 -> NumV (op n1 n2)


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


(** Parsing **)

(* we use CCsexp as a library for s-expressions *)
open CCSexp

(*
  Instead of using a standard algebraic data types for sexps, 
  this library uses a feature known as "polymorphic variants".
  
  This is a flexible mechanism where data is build by tagging it with symbols,
  instead of using pre-declared constructors. These symbols are written with ticks `.
  
  Then sexp is just an alias for a (recursive) polymorphic variant:

    type sexp = [ `Atom of id | `List of 'a list ] as 'a

  When matching such an sexp, we look at the tag. See the parse function below for an example.
  You can also look at the definition and implementation of the CCsexp module for more details.
*)

let rec parse (sexp : sexp) : exp = (* it's not a problem to have a variable [sexp] of type [sexp] (separate namespaces) *)
  match sexp with
  | `Atom s ->
    begin match int_of_string_opt s with (* A frequent trouble: when nesting [match] the inner match must *)
      | Some n -> Num n                  (* be enclosed in [( ... )] or [begin ... end] *)
      | None -> Var s
    end
  | `List [`Atom "+" ; e1 ; e2 ] -> Plus (parse e1, parse e2)
  | `List [`Atom "*" ; e1 ; e2 ] -> Times (parse e1, parse e2)
  | e -> Fmt.failwith "Not a valid exp: %a" CCSexp.pp e

let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse file %s: %s" filename msg