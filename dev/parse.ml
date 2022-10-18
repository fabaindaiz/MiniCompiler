(** Parser **)
open Ast
open Printf
open CCSexp

exception CTError of string


(* parse a CSSexp to an e expresion *)
let rec parse_exp (sexp : sexp) : expr =
  match sexp with
  | `Atom "true" -> Bool true
  | `Atom "false" -> Bool false
  | `Atom s -> (
    match Int64.of_string_opt s with Some n -> Num n | None -> Id s )
  | `List (`Atom "tup" :: exprs) -> Tuple (List.map parse_exp exprs)
  | `List [eop; e] -> (
    match eop with 
    | `Atom "add1" -> Prim1 (Add1, parse_exp e)
    | `Atom "sub1" -> Prim1 (Sub1, parse_exp e)
    | `Atom "not" -> Prim1 (Not, parse_exp e)
    | `Atom name -> App (name, [parse_exp e])
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | `List [eop; e1; e2] -> 
    (match eop with
    | `Atom "let" -> 
      (match e1 with
      | `List [`Atom id; e] -> Let (id, parse_exp e, parse_exp e2)
      | _ -> raise (CTError (sprintf "Not a valid let assignment: %s" (to_string e1))) )
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
    | `Atom "get" -> Prim2 (Get, parse_exp e1, parse_exp e2)
    | `Atom name -> App (name, [parse_exp e1 ; parse_exp e2])
    | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp))) )
  | `List [`Atom "if"; e1; e2; e3] -> If (parse_exp e1, parse_exp e2, parse_exp e3)
  | `List [ `Atom "set"; e; k; v ] -> Set (parse_exp e, parse_exp k, parse_exp v)
  | `List (`Atom name :: e2) -> App (name, List.map parse_exp e2)
  | _ -> raise (CTError (sprintf "Not a valid expr: %s" (to_string sexp)))

let rec parse_prog (sexp : sexp) : prog =
  match sexp with
  | `List (hd :: tl) -> (
    match hd, tl with
    | `List [`Atom "def" ; `List (`Atom name :: args) ; body], _ ->
      let (funcdefs, expr) = parse_prog (`List tl) in
      let arg_names = List.map parse_arg_name args in
      [ DefFun (name, arg_names, parse_exp body) ] @ funcdefs, expr
    | `List (`Atom "defsys" :: `Atom name :: arg_spec), _ -> (
      match List.rev arg_spec with
      | (ret :: `Atom "->" :: args) -> 
        let (funcdefs, expr) = parse_prog (`List tl) in
        let arg_types = List.map parse_c_type (List.rev args) in
        let ret_type = parse_c_type ret in
        [ DefSys (name, arg_types, ret_type) ] @ funcdefs, expr
      | _ -> raise (CTError (sprintf "Not a valid type declaration: %s" (to_string (`List arg_spec))))
      )
    | _, [] -> [], parse_exp hd
    | _ -> [], parse_exp sexp
  )
  | _ -> [], parse_exp sexp

and parse_arg_name (sexp : sexp) : string =
  match sexp with
  | `Atom name -> name
  | _ -> raise (CTError (sprintf "Not a valid argument name: %s" (to_string sexp)))

and parse_c_type (sexp : sexp) : ctype =
  match sexp with
  | `Atom "any" -> CAny
  | `Atom "int" -> CInt
  | `Atom "bool" -> CBool
  | _ -> raise (CTError (sprintf "Not a valid type declaration: %s" (to_string sexp)))


(* parse a program from a file *)
let sexp_from_file : string -> CCSexp.sexp =
 fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> raise (CTError (sprintf "Unable to parse file %s: %s" filename msg))

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> raise (CTError (sprintf "Unable to parse string %s: %s" src msg))
