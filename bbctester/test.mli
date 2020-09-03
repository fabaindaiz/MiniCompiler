
(** Expected status of a test *)
type status =
  | CTError
    (* Compile-time error *)
  | RTError
    (* Run-time error *)
  | NoError
    (* No error *)

val status_of_string : string -> status
val string_of_status : status -> string


(** Ocaml representation of test files
 * All strings are enforced to be trimmed.
 * The expected string *)
type t =
  { name : string
  ; description : string
  ; params : string list
  ; status : status
  ; src : string
  ; expected : string }

val read_test : string -> t option

type compiler = Format.formatter -> string -> unit

val make_test : string -> compiler:compiler -> string -> string * (unit -> unit)
val testfiles_in_dir : string -> string list

val tests_from_dir : ?runtime:string ->
  compiler:compiler ->
  string -> (string * unit Alcotest.test_case list) list
