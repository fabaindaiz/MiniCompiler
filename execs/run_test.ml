open Dev.Ast
open Dev.Parse
open Dev.Compile
open Dev.Interp
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
  check exp "same var" (parse_exp (`Atom "x")) (Id "x")

let test_parse_bool () =
  check exp "same bool" (parse_exp (`Atom "true")) (Bool true)
  
let test_parse_add1 () =
  check exp "increment applies" 
  (parse_exp (`List [`Atom "add1" ; `Atom "1"])) 
  (Prim1 (Add1, Num 1L))

let test_parse_sub1 () =
  check exp "decrement applies" 
  (parse_exp (`List [`Atom "sub1" ; `Atom "1"])) 
  (Prim1 (Sub1, Num 1L))
  
let test_parse_add () =
  check exp "addition applies" 
  (parse_exp (`List [`Atom "+" ; `Atom "1" ; `Atom "7"])) 
  (Prim2 (Add, Num 1L, Num 7L))

let test_parse_less () =
  check exp "lesser comparison applies" 
  (parse_exp (`List [`Atom "<=" ; `Atom "1" ; `Atom "7"])) 
  (Prim2 (Lte, Num 1L, Num 7L))

let test_parse_and () =
  check exp "conjunction applies" 
  (parse_exp (`List [`Atom "and" ; `Atom "true" ; `Atom "false"])) 
  (Prim2 (And, Bool true, Bool false))

let test_parse_fork () =
  check exp "if clause applies"
  (parse_exp (`List [`Atom "if" ; `Atom "true" ; `Atom "1" ; `Atom "0"])) 
  (If (Bool true, Num 1L, Num 0L))

let test_parse_let () =
  check exp "declaration applies"
  (parse_exp (`List [`Atom "let" ; `List [`Atom "x" ; `Atom "1"] ; `List [`Atom "let" ; `List [`Atom "y" ; `Atom "7"] ; `Atom "10"] ])) 
  (Let ("x", Num 1L, Let ("y", Num 7L, Num 10L)))

let test_parse_compound () =
  check exp "same expr"
  (parse_exp (`List [`Atom "+" ; `List [`Atom "+" ; `Atom "3"; `Atom "x"]; `Atom "7"]))
  (Prim2 (Add, Prim2 (Add, Num 3L, Id "x"), Num 7L))

let test_parse_error () =
  let sexp = `List [`List [`Atom "foo"]; `Atom "bar"] in
  check_raises "Should raise a parse error" 
    (CTError (Fmt.str "Not a valid expr: %a" CCSexp.pp sexp))
    (fun () -> ignore @@ parse_exp sexp)

(* Tests for our [interp] function *)
let test_interp_num () =
  check value "same int" (interp (Num 42L) empty_env empty_fenv) (NumV 42L)

let test_interp_var () =
  check value "same int" (interp (Id "x") (extend_env ["x"] [(NumV 7L)] empty_env) empty_fenv) (NumV 7L)

let test_interp_bool () =
  let v = (interp (Bool true) empty_env empty_fenv) in check value "same bool" v 
  (BoolV true)

let test_interp_add1 () =
  let v = (interp (Prim1 (Add1, Num 1L)) empty_env empty_fenv) in 
  check value "correct increment" v 
  (NumV 2L)

let test_interp_sub1 () =
  let v = (interp (Prim1 (Sub1, Num 1L)) empty_env empty_fenv) in 
  check value "correct decrement" v 
  (NumV 0L)

let test_interp_add () =
  let v = (interp (Prim2 (Add, Num 1L, Num (-1L))) empty_env empty_fenv) in 
  check value "correct addition" v 
  (NumV 0L)

let test_interp_less () =
  let v = (interp (Prim2 (Lte, Num 70L, Num 10L))) empty_env empty_fenv in 
  check value "correct lesser comparison" v 
  (BoolV false)

let test_interp_and () =
  let v = (interp (Prim2 (And, Bool true, Bool false))) empty_env empty_fenv in 
  check value "correct conjunction" v 
  (BoolV false)

let test_interp_fork_1 () =
  let v = (interp (If (Prim2 (Lte, Num 0L, Num 1L), (Num 70L), (Bool false)))) empty_env empty_fenv in 
  check value "correct execution fork true" v 
  (NumV 70L)
  
let test_interp_fork_2 () =
  let v = (interp (If (Prim2 (Lte, Num 1L, Num 0L), (Num 70L), (Bool false)))) empty_env empty_fenv in 
  check value "correct execution fork false" v 
  (BoolV false)
  
let test_interp_let_1 () =
  let v = (interp (Let ("x", Num 1L, (Let ("y", Bool false, (Prim2 (And, Id "y", (Prim2 (Lte, Id "x", Num 0L))))))))) empty_env empty_fenv in
  check value "correct simple variable assignment" v 
  (BoolV false)
  
let test_interp_let_2 () =
  let v = (interp (Let ("x", Num 2L, (Let ("y", (Let ("x", Num 1L, Id "x")), (Prim2 (Add, Id "x", Id "x"))))))) empty_env empty_fenv in
  check value "correct complex variable assignment" v 
  (NumV 4L)

let test_interp_fo_fun_1 () =
  let v = (interp_prog (
    [DefFun ("f", ["x"], (Prim2 (Add, Id "x", Id "x")))],
    (App ("f", [Num 2L]))
  )) empty_env in 
  check value "correct simple function execution" v 
  (NumV 4L)
  
let test_interp_fo_fun_2 () =
  let v = (interp_prog (
    [DefFun ("f", ["x" ; "y" ; "z"], (Prim2 (Add, (Prim2 (Add, Id "x", Id "y")), Id "z")))],
  (App ("f" , [Num 2L ; Num 20L ; Num 200L]))
  )) empty_env in 
  check value "correct complex function execution" v 
  (NumV 222L)
  
let test_interp_fo_app_1 () =
  check value "correct simple function application"
  (NumV 14L)
  (interp_prog ( 
  (
    [
      DefFun ("f", ["x" ; "y"], (Prim2 (Add, Id "x", Id "y")));
      DefFun ("g", ["y"], (Prim2 (Add, Num 7L, Id "y")))
    ],
    (App ("g", [
      (App ("f", [Num 4L ; Num 3L]))
    ]))
  )
  ) empty_env)

let test_interp_fo_app_2 () =
  check value "correct simple function application"
  (NumV 200L)
  (interp_prog ( 
  (
    [
      DefFun ("f", ["x" ; "y"], (Prim2 (Add, Id "x", Id "y")));
      DefFun ("g", ["x"], (App ("f", [Id "x" ; Id "x"])))
    ],
    (App ("g", [Num 100L]))
  )
  ) empty_env)

let test_interp_compound () =
  check value "same int"
    (interp (Prim2 (Add, Prim2 (Add, Num 3L, (Prim1 (Sub1, Num 6L))), Num 12L)) empty_env empty_fenv)
    (NumV 20L)


let test_error_III () =
  let v = (fun () -> ignore @@ interp (Prim2 (Add, Bool true,  Num (-1L))) empty_env empty_fenv) in 
  check_raises "incorrect addition" 
  (RTError "Expected two integers, but got true and -1") v


let test_error_BBB () =
  let v = (fun () -> ignore @@ (interp (Prim2 (And, Num 5L, Bool false))) empty_env empty_fenv) in 
  check_raises "incorrect conjunction"
  (RTError "Expected two booleans, but got 5 and false") v

let test_error_IIB () =
  let v = (fun () -> ignore @@ (interp (Prim2 (Lte, Bool true, Num 10L))) empty_env empty_fenv) in 
  check_raises  "incorrect lesser comparison" 
  (RTError "Expected two integers, but got true and 10") v

(* OCaml tests: extend with your own tests *)
let ocaml_tests = [
  "parse", [
    test_case "A number" `Quick test_parse_int ;
    test_case "A variable" `Quick test_parse_var ;
    test_case "A boolean" `Quick test_parse_bool ;
    test_case "An increment" `Quick test_parse_add1 ;
    test_case "A decrement" `Quick test_parse_sub1 ;
    test_case "An addition" `Quick test_parse_add ;
    test_case "A lesser comparison" `Quick test_parse_less ;
    test_case "A conjunction" `Quick test_parse_and ;
    test_case "An if clause" `Quick test_parse_fork ;
    test_case "A definition" `Quick test_parse_let ;
    test_case "A compound expression" `Quick test_parse_compound ;
    test_case "An invalid s-expression" `Quick test_parse_error
  ] ;
  "interp", [
    test_case "A number" `Quick test_interp_num ;
    test_case "A variable" `Quick test_interp_var ;
    test_case "A boolean" `Slow test_interp_bool ;
    test_case "An increment" `Slow test_interp_add1 ;
    test_case "An decrement" `Slow test_interp_sub1 ;
    test_case "An addition" `Slow test_interp_add ;
    test_case "A lesser comparison" `Slow test_interp_less ;
    test_case "A conjunction" `Slow test_interp_and ;
    test_case "An if clause when true" `Slow test_interp_fork_1 ;
    test_case "An if clause when false" `Slow test_interp_fork_2 ;
    test_case "A simple definition" `Slow test_interp_let_1 ;
    test_case "A complex definition" `Slow test_interp_let_2 ;
    test_case "A simple function" `Slow test_interp_fo_fun_1 ;
    test_case "A complex function" `Slow test_interp_fo_fun_2 ;
    test_case "A simple application" `Slow test_interp_fo_app_1 ;
    test_case "A complex application" `Slow test_interp_fo_app_2 ;
    test_case "A compound expression" `Quick test_interp_compound
  ] ;
  "errors", [
      test_case "Addition of true" `Quick test_error_III ;
    test_case "And of 5" `Quick test_error_BBB ;
    test_case "Lesser than true" `Quick test_error_IIB


  ]
]     

(* Entry point of tester *)
let () =
  (* BBC tests: don't change the following, simply add .bbc files in the bbctests/ directory *)
  let bbc_tests = 
    let compile_flags = Option.value (Sys.getenv_opt "CFLAGS") ~default:"-g" in
    let compiler : string -> out_channel -> unit = 
      fun s o -> fprintf o "%s" (compile_prog (parse_prog (sexp_from_string s))) in
    let oracle : string -> status * string = (
      fun s -> (
        try
          NoError, string_of_val (interp_prog (parse_prog (sexp_from_string s)) empty_env)
        with
        | RTError msg -> RTError, msg
        | CTError msg -> CTError, msg
        |  e -> RTError, "Oracle raised an unknown error :"^ Printexc.to_string e 
      )
    ) in
    tests_from_dir ~compile_flags ~compiler ~oracle ~runtime:"rt/sys.c" "bbctests" in
  run "Tests entrega 1" (ocaml_tests @ bbc_tests)
