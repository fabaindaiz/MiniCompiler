open Dev.Ast
open Dev.Parse
open Dev.Interp
open Dev.Compile
open Alcotest
open Bbctester.Test
open Printf

(* Testing arithmetic expression using the print function defined in Interp 
   and the default equality for comparison *)
let exp : expr testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_expr e)) (=)

let value : value testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_val e)) (=)


(* Tests for our [parse] function *)
let test_parse_int () =
  check exp "same int" (parse_exp (`Atom "5")) (Num 5L)

let test_parse_var () =
  check exp "same var" (parse_exp (`Atom "x")) (Var "x")

let test_parse_compound () =
  check exp "same expr"
    (parse_exp (`List [`Atom "+" ; `List [`Atom "+" ; `Atom "3"; `Atom "x"]; `Atom "7"]))
    (Prim2 (Add, Prim2 (Add, Num 3L, Var "x"), Num 7L))

let test_parse_error () =
  let sexp = `List [`Atom "foo"; `Atom "bar"] in
  check_raises "Should raise failwith" 
    (Failure (Fmt.strf "Not a valid expr: %a" CCSexp.pp sexp))
    (fun () -> ignore @@ parse_exp sexp)

(* Tests for our [interp] function *)
let test_interp_num () =
  check value "same int" (interp (Num 42L) empty_env) (NumV 42L)

let test_interp_var () =
  check value "same int" (interp (Var "x") ["x", NumV 7L]) (NumV 7L)

let test_interp_compound () =
  check value "same int"
    (interp (Prim2 (Add, Prim2 (Add, Num 3L, Num 5L), Num 12L)) empty_env)
    (NumV 20L)

(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parse", [
    test_case "A number" `Quick test_parse_int ;
    test_case "A variable" `Quick test_parse_var ;
    test_case "A compound expression" `Quick test_parse_compound ;
    test_case "An invalid s-expression" `Quick test_parse_error
  ] ;
  "interp", [
    test_case "A number" `Quick test_interp_num ;
    test_case "A variable" `Quick test_interp_var ;
    test_case "A compound expression" `Quick test_interp_compound
  ]
]     

(* Entry point of tester *)
let () =
  (* BBC tests: don't change the following, simply add .bbc files in the bbctests/ directory *)
  let bbc_tests = 
    let compile_flags = Option.value (Sys.getenv_opt "CFLAGS") ~default:"-g" in
    let compiler : string -> out_channel -> unit = 
      fun s o -> fprintf o "%s" (compile (parse_exp (sexp_from_string s))) in
    let interpreter : string -> string = 
      fun s -> string_of_val (interp (parse_exp (sexp_from_string s)) empty_env) in
    tests_from_dir ~compile_flags ~compiler ~interpreter ~runtime:"rt/sys.c" "bbctests" in
  run "Tests entrega 1" (ocaml_tests @ bbc_tests)
