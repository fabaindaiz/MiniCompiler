open Compiler
open Expr
open Compile
open Alcotest

let expr : expr testable =
  testable pp_expr (=)

let parse_tests =
  let open Parse in

  (* Tests for our [parse] function *)
  let test_parse_int () =
    check expr "same int" (parse (`Atom "5")) (Num 5L)
  in


  let test_parse_compound () =
    check expr "same expr"
      (parse (`List [`Atom "add1" ; `List [`Atom "sub1" ; `Atom "3"; ]]))
      (Add1 (Sub1 (Num 3L)))
  in

  (* An example of a test catching an error *)
  let test_parse_error () =
    let sexp = `List [`Atom "foo"; `Atom "bar"] in
    check_raises "Should raise failwith" 
      (Failure (Fmt.strf "Not a valid exp: %a" CCSexp.pp sexp))
      (fun () -> ignore @@ parse sexp)
  in

  "parse", [
    test_case "A number" `Quick test_parse_int ;
    test_case "A compound expression" `Quick test_parse_compound ;
    test_case "An invalid s-expression" `Quick test_parse_error
  ]


let interp_tests =
  let open Interp in

  let value : value testable =
    testable pp_value (=)
  in

  (* Tests for our [interp] function *)
  let test_interp_num () =
    check value "same int" (interp (Num 42L)) (NumV 42L)
  in

  let test_interp_compound () =
    check value "same int"
      (interp (Add1 (Add1 (Num 40L))))
      (NumV 42L)
  in

  "interp", [
    (* Use the `Slow parameter for tests that only need to be run with the full test suite
      The tests here only concern the interpreter, so we tag them as slow.
      Set the ALCOTEST_QUICK_TESTS environment variable (to =1 for instance) to disable slow tests. *)
    test_case "A number" `Slow test_interp_num ;
    test_case "A compound expression" `Slow test_interp_compound
  ]


let interpreter (src : string) : string =
  let open Interp in
  let e = Parse.(parse (sexp_from_string src)) in
  Fmt.to_to_string pp_value (interp e)

(* Entry point of tests
 * Beware that the [Alcotest] library takes control of all command line
 * arguments in [run].
 * See the documentation at https://github.com/mirage/alcotest *)
let () =
  run "Compiler" @@
    [ parse_tests ; interp_tests ]
    @ Bbctester__Test.tests_from_dir 
        ~runtime:"compiler/rtsys.c" 
        ~compiler:compile_src 
        ~interpreter "tests"