
type status =
  | CTError
  | RTError
  | NoError

let status_of_string = function
  | "fail"
  | "RT error" -> RTError
  | "CT error" -> CTError
  | _ -> NoError

let string_of_status = function
  | RTError -> "RT error"
  | CTError -> "CT error"
  | NoError -> "No error"

(** Ocaml representation of test files *)
(* All strings are enforced to be trimmed. *)
(* The expected string *)
type t =
  { name : string
  ; description : string
  ; params : string list
  ; status : status
  ; src : string
  ; expected : string }

let test_regexp =
  Str.regexp "NAME:\\|DESCRIPTION:\\|PARAMS:\\|STATUS:\\|SRC:\\|EXPECTED:\\|END"


let read_test filename =
  if Sys.file_exists filename
  then
    let content = CCIO.(with_in filename read_all) in
    let open Str in
    let get_opt s dflt = function
      | Delim s' :: Text content :: rest when s = s' ->
        String.trim content, rest
      | all -> dflt, all
    in
    let toks = full_split test_regexp content in
    let name, toks = get_opt "NAME:" Filename.(chop_extension @@ basename filename) toks in
    let description, toks = get_opt "DESCRIPTION:" "" toks in
    let params, toks =
      let params_string, toks = get_opt "PARAMS:" "" toks in
      String.(List.map trim @@ split_on_char ',' params_string), toks
    in
    let status, toks = get_opt "STATUS:" "ok" toks in
    match toks with
    | Delim "SRC:" :: Text src ::
      Delim "EXPECTED:" :: Text expected :: ( [] | Delim "END" :: _ ) ->
      Some { name ; description ; params ; status = status_of_string status ;
           src ; expected = String.trim expected }
    | _ ->
      Printf.fprintf stderr "Wrong format in test file %s" filename;
      None
  else
    (Printf.fprintf stderr "Test file %s not found." filename ; None)


let (let*) = Result.bind

let string_match =
  let open Alcotest in
  let matches pat s =
    try let _ = Str.(search_forward (regexp pat) s 0) in true
    with Not_found -> false
  in
  testable (pp string) matches

let status =
  let open Alcotest in
  testable Fmt.(using string_of_status string) (=)


(** Test pairs giving access to the first component when testing the second component *)
let dep_pair : type a b. a Alcotest.testable -> (a -> b Alcotest.testable) -> (a * b) Alcotest.testable =
  fun cmp1 cmp2 ->
  let open Alcotest in
  let cmp_pair (x1, x2) (y1, y2) = equal cmp1 x1 y1 && equal (cmp2 x1) x2 y2 in
  testable (fun fmt p -> pp (pair cmp1 (cmp2 (fst p))) fmt p) cmp_pair

let compare_results =
  let cmp_res = function
    | NoError -> Alcotest.string
    | _ -> string_match
  in
  dep_pair status cmp_res

let filter_lines pred s =
  CCString.split ~by:"\n" s
  |> List.filter pred
  |> String.concat "\n"

let is_comment_line s =
  not (CCString.prefix ~pre:"|" s)

let process_output out =
  String.trim out |> filter_lines is_comment_line

type compiler = Format.formatter -> string -> unit

let bin_format =
  let out, _ , _ = CCUnix.call "uname -s" in
  let arch = String.trim out in
  match arch with
  | "Linux" -> "elf64"
  | "Darwin" -> "macho64"
  | _ -> Fmt.failwith "Unknown architecture %s" arch

let wrap_result (out, err, retcode) =
  if retcode = 0 then Ok () else Error (CTError, out ^ err)

let clang runtime basefile =
  wrap_result @@ CCUnix.call "clang -o %s.run %s %s.o" basefile runtime basefile

let nasm basefile =
  wrap_result @@ CCUnix.call "nasm -f %s -o %s.o %s.s" bin_format basefile basefile

let make_test
    runtime
    ~(compiler:compiler)
    ~interpreter
    filename =
  match read_test filename with
  | None -> Alcotest.failf "Could not open or parse test %s" filename
  | Some test ->
    let exec () =
      let base = Filename.chop_extension filename in
      let exe = base ^ ".run" in

      let res =
        let* () =
          try
            let compile oc = compiler (Format.formatter_of_out_channel oc) test.src
            in Ok (CCIO.with_out (base ^ ".s") compile) (* actually no need to go through a file... *)
          with e -> Error (CTError, Printexc.to_string e)
        in
        let* () = nasm base in
        let* () = clang runtime base in
        let out, err, retcode = CCUnix.call ~env:(Array.of_list test.params) "./%s" exe in
        if retcode = 0 then
          Ok (process_output out)
        else Error (RTError, out ^ err)
      in

      let res = match res with
        | Ok out -> NoError, out
        | Error err -> err
      in

      let expected =
        match interpreter with
        | Some interp when test.status = NoError && test.expected = "|INTERPRET" ->
          NoError, interp test.src
        | _ -> test.status, test.expected
      in

      let open Alcotest in
      check compare_results test.name expected res

    in test.name, exec



let testfiles_in_dir dir =
  CCUnix.with_process_in ("find " ^ dir ^ " -name '*.test'") ~f:CCIO.read_lines_l

let name_from_file filename =
  let open Filename in
  dirname filename ^ "::" ^ basename (chop_extension filename)

let tests_from_dir ~runtime ~compiler ?interpreter dir =
  let open Alcotest in
  let to_test testfile =
    let testname, exec_test = make_test runtime ~compiler ~interpreter testfile in
    name_from_file testfile, [test_case testname `Quick exec_test]
  in
  List.map to_test @@ testfiles_in_dir dir
  |> CCList.sort (fun (s1,_) (s2,_) -> String.compare s1 s2)

(* Use as follow: *)
(* run "Tests" @@ List.map tests_from_dir [ "failing"; "tests"] *)


