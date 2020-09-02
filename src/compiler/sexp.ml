open Fmt

type sexp =
  | Atom : string -> sexp
  | List : sexp list -> sexp

let rec pp_sexp : sexp Fmt.t =
  fun fmt sexp ->
  match sexp with
  | Atom s -> string fmt s
  | List l -> parens (list pp_sexp) fmt l

(* Conversion function from CCSexp internal type *)
let rec to_sexp =
  function `Atom s -> Atom s | `List l -> List (List.map to_sexp l)
