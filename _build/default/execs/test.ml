open Dev.Ast
open Dev.Parse
open Dev.Interp
open Dev.Lib
open Alcotest

(* Testing arithmetic expression using the print function defined in Interp 
   and the default equality for comparison *)
let exp : exp testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_exp e)) (=)

let value : value testable =
  testable (fun oc e -> Format.fprintf oc "%s" (string_of_val e)) (=)


(* Tests for our [parse] function *)
let test_parse_int () =
  check exp "same int" (parse (`Atom "5")) (Num 5)

let test_parse_var () =
  check exp "same var" (parse (`Atom "x")) (Var "x")

let test_parse_compound () =
  check exp "same expr"
    (parse (`List [`Atom "+" ; `List [`Atom "*" ; `Atom "3"; `Atom "x"]; `Atom "7"]))
    (Plus (Times (Num 3, Var "x"), Num 7))

let test_parse_error () =
  let sexp = `List [`Atom "foo"; `Atom "bar"] in
  check_raises "Should raise failwith" 
    (Failure (Fmt.strf "Not a valid exp: %a" CCSexp.pp sexp))
    (fun () -> ignore @@ parse sexp)

(* Tests for our [interp] function *)
let test_interp_num () =
  check value "same int" (interp empty_env (Num 42)) (NumV 42)

let test_interp_var () =
  check value "same int" (interp ["x", NumV 7] (Var "x")) (NumV 7)

let test_interp_compound () =
  check value "same int"
    (interp empty_env (Plus (Times (Num 3, Num 5), Num 12)))
    (NumV 27)

(* Entry point of tests
 * Beware that the [Alcotest] library takes control of all command line
 * arguments in [run].
 * See the documentation at https://github.com/mirage/alcotest *)
let () =
  run "A Simple Interpreter"
    [
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