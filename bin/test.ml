open Compiler.Compile
open Alcotest

let parse_exp_tests =
  let open Compiler.Interp in
  let open Compiler.Parse in
  (* Testing arithmetic expression using the print function defined in Interp 
    and the default equality for comparison *)
  let exp : exp testable =
    testable pp_exp (=)
  in

  (* Tests for our [parse] function *)
  let test_parse_exp_int () =
    check exp "same int" (parse_exp (`Atom "5")) (Num 5)
  in

  let test_parse_exp_var () =
    check exp "same var" (parse_exp (`Atom "x")) (Var "x")
  in

  let test_parse_exp_compound () =
    check exp "same expr"
      (parse_exp (`List [`Atom "+" ; `List [`Atom "*" ; `Atom "3"; `Atom "x"]; `Atom "7"]))
      (Plus (Times (Num 3, Var "x"), Num 7))
  in

  (* An example of a test catching an error *)
  let test_parse_exp_error () =
    let sexp = `List [`Atom "foo"; `Atom "bar"] in
    check_raises "Should raise failwith" 
      (Failure (Fmt.strf "Not a valid exp: %a" CCSexp.pp sexp))
      (fun () -> ignore @@ parse_exp sexp)
  in

  "parse", [
    (* Use the `Slow parameter for tests that only need to be run with the full test suite
      The tests here only concern the interpreter, so we tag them as slow.
      Set the ALCOTEST_QUICK_TESTS environment variable (to =1 for instance) to disable slow tests. *)
    test_case "A number" `Slow test_parse_exp_int ;
    test_case "A variable" `Slow test_parse_exp_var ;
    test_case "A compound expression" `Slow test_parse_exp_compound ;
    test_case "An invalid s-expression" `Slow test_parse_exp_error
  ]


let interp_tests =
  let open  Compiler.Interp in

  let value : value testable =
    testable pp_value (=)
  in

  (* Tests for our [interp] function *)
  let test_interp_num () =
    check value "same int" (interp empty_env (Num 42)) (NumV 42)
  in

  let test_interp_var () =
    check value "same int" (interp ["x", NumV 7] (Var "x")) (NumV 7)
  in

  let test_interp_compound () =
    check value "same int"
      (interp empty_env (Plus (Times (Num 3, Num 5), Num 12)))
      (NumV 27)
  in

  "interp", [
    test_case "A number" `Slow test_interp_num ;
    test_case "A variable" `Slow test_interp_var ;
    test_case "A compound expression" `Slow test_interp_compound
  ]

(* Entry point of tests
 * Beware that the [Alcotest] library takes control of all command line
 * arguments in [run].
 * See the documentation at https://github.com/mirage/alcotest *)
let () =
  run "Compiler" @@
    [ parse_exp_tests ; interp_tests ]
    @ Bbctester__Test.tests_from_dir ~runtime:"compiler/rtsys.c" ~compiler:compile_src ~dir:"tests"