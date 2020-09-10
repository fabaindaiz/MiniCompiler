(** Parsing **)

(* we use CCsexp as a library for s-expressions *)
open CCSexp

(*
  Instead of using a standard algebraic data types for sexps, such as:
  
      type sexp = Atom of string | List of sexp list 
 
  this library uses a feature known as "polymorphic variants".
  This is a flexible mechanism where data is built by tagging it with symbols,
  instead of using pre-declared constructors. These symbols are written with ticks `.
  
  Then sexp is just an alias for a (recursive) polymorphic variant:

    type sexp = [ `Atom of id | `List of 'a list ] as 'a

  When matching such an sexp, we look at the tag. See the parse function below for an example.
  You can also look at the definition and implementation of the CCsexp module for more details.
*)

let sexp_from_string (src : string) : CCSexp.sexp =
  match CCSexp.parse_string src with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse src %s: %s" src msg

let sexp_from_file (filename : string) : CCSexp.sexp =
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> Fmt.failwith "Unable to parse file %s: %s" filename msg



open Expr

let rec parse (sexp : sexp) : expr = 
  match sexp with
  | `Atom s ->
    begin match Int64.of_string_opt s with
      | Some n -> Num n
      | None -> Fmt.failwith "Not a known atom: %s" s
    end
  | `List [`Atom "add1" ; e ] -> Add1 (parse e) 
  | `List [`Atom "sub1" ; e ] -> Sub1 (parse e)
  | e -> Fmt.failwith "Not a valid exp: %a" CCSexp.pp e
