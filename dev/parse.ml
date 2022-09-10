(** Parser **)
open Ast
open Printf
open CCSexp

(* parse a CSSexp to an expresion *)
let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `Atom "true" -> Bool true
  | `Atom "false" -> Bool false
  | `Atom s -> (
    match Int64.of_string_opt s with Some n -> Num n | None -> Id s )
  | `List [eop; e] -> (
    match eop with 
    | `Atom "add1" -> Prim1 (Add1, parse_exp e)
    | `Atom "sub1" -> Prim1 (Sub1, parse_exp e)
    | `Atom "not" -> Prim1 (Not, parse_exp e)
    | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp)) )
  | `List [eop; e1; e2] -> 
    (match eop with
    | `Atom "let" -> 
      (match e1 with
      | `List [`Atom id; e] -> Let (id, parse_exp e, parse_exp e2)
      | _ -> failwith "parse error in let" )
    | `Atom "+" -> Prim2 (Add, parse_exp e1, parse_exp e2)
    | `Atom "-" -> Prim2 (Sub, parse_exp e1, parse_exp e2)
    | `Atom "*" -> Prim2 (Mul, parse_exp e1, parse_exp e2)
    | `Atom "/" -> Prim2 (Div, parse_exp e1, parse_exp e2)
    | `Atom "and" -> Prim2 (And, parse_exp e1, parse_exp e2)
    | `Atom "or" -> Prim2 (Or, parse_exp e1, parse_exp e2)
    | `Atom "<" -> Prim2 (Lt, parse_exp e1, parse_exp e2)
    | `Atom ">" -> Prim2 (Gt, parse_exp e1, parse_exp e2)
    | `Atom "<=" -> Prim2 (Lte, parse_exp e1, parse_exp e2)
    | `Atom ">=" -> Prim2 (Gte, parse_exp e1, parse_exp e2)
    | `Atom "=" -> Prim2 (Eq, parse_exp e1, parse_exp e2)
    | `Atom "!=" -> Prim2 (Neq, parse_exp e1, parse_exp e2)
    | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp)) )
  | `List [`Atom "if"; e1; e2; e3] -> If (parse_exp e1, parse_exp e2, parse_exp e3)
  | _ -> failwith (sprintf "Not a valid expr: %s" (to_string sexp))

(* parse a program from a file *)
let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)

(* parse a program from a string *)
let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse src %s: %s" src msg)
