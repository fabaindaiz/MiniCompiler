(** Parser **)

open Ast
open Printf
(* we use CCsexp as a library for s-expressions *)
open CCSexp

(*
  Instead of using a standard algebraic data types for sexps, such as:
  
      type sexp = Atom of string | List of sexp list 
 
  this library uses a feature known as "polymorphic variants".
  This is a flexible mechanism where data is built by tagging it with symbols,
  instead of using pre-declared constructors. These symbols are written with ticks `.
  
  Then sexp is just an alias for a (recursive) polymorphic variant:

    type sexp = [ `Atom of string | `List of 'a list ] as 'a

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
  | _ -> failwith (sprintf "Not a valid exp: %s" (to_string sexp))

let sexp_from_file : string -> CCSexp.sexp =
  fun filename ->
  match CCSexp.parse_file filename with
  | Ok s -> s
  | Error msg -> failwith (sprintf "Unable to parse file %s: %s" filename msg)
