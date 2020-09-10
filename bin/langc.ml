open Compiler.Compile

let (let*) x f = x f
let (let+) = Result.bind



let wrap_result (out, err, retcode) =
  if retcode = 0 then Ok () else Error (out ^ err)
let bin_format =
  let out, _ , _ = CCUnix.call "uname -s" in
  let arch = String.trim out in
  match arch with
  | "Linux" -> "elf64"
  | "Darwin" -> "macho64"
  | _ -> Fmt.failwith "Unknown architecture %s" arch
  
let clang runtime basefile =
  wrap_result @@ CCUnix.call "clang -o %s.run %s %s.o" basefile runtime basefile

let nasm basefile =
  wrap_result @@ CCUnix.call "nasm -f %s -o %s.o %s.s" bin_format basefile basefile

let with_in s f = CCIO.with_in s f
  (* if s = "-" then f stdin else CCIO.with_in s f *)

let with_out s f =
  if s = "-" then f stdout else CCIO.with_out s f

let compile_to_asm src asm_filename =
  let* cout = with_out asm_filename in
  compile_src (Format.formatter_of_out_channel cout) src

let compile_to_exe filename src exe_filename =
  let* temp_dir = CCUnix.with_temp_dir "langc_tmp_dir" in
  let filename = Filename.(temp_file ~temp_dir (basename filename) "") in 
  let () =
    let* asm_cout = CCIO.with_out (filename ^ ".s") in
    compile_src (Format.formatter_of_out_channel asm_cout) src
  in
  let res =
    let+ () = nasm filename in
    clang "compiler/rtsys.c" filename
  in
  match res with 
  | Ok () -> 
    let* cout = CCIO.with_out ~mode:0o744 exe_filename in
    CCIO.with_in (filename ^ ".run") @@ Fun.flip CCIO.copy_into cout
  | Error err -> 
    Printf.fprintf stderr "%s\nCompilation of %s unsucessful" err filename ; 
    exit 1



(* main *)
let () =
  let in_file = ref "" in
  let out_file = ref "" in
  let only_asm = ref false in
  let src = ref "" in
  let spec =
    Arg.align 
    [ "-s", Arg.Set only_asm, "\tOnly outputs the generated assembly code"
    ; "-o", Arg.Set_string out_file, "\tSet the name of the generated file (use '-' for stdout; defaults to srcfile with appropriate extension, `a` if srcfile is not provided)"
    ; "--", Arg.Rest (fun s -> src := s), "\tRead the source on stdin rather than from a file"]
  in
  let usage_msg = "usage: langc [OPTIONS] (srcfile | -- srcs)" in
  let exit_err err_msg =
    Printf.fprintf stderr "%s%s" err_msg (Arg.usage_string spec usage_msg) ;
    exit 2
  in
  let annon s = 
    if !in_file = "" then in_file := s 
    else exit_err (Printf.sprintf "Multiple source files provided: %s, %s\n" !in_file s) 
  in
  Arg.parse spec annon usage_msg ;

  let src = 
    if !in_file = "" then 
      if !src = "" then 
        exit_err "srcfile not provided!\n"
      else !src
    else if not (Sys.file_exists !in_file)
    then exit_err (!in_file ^ " does not exists!\n")
    else let* cin = with_in !in_file in CCIO.read_all cin
  in

  let file = if !in_file = "" then "a" else !in_file in

  let out_file = 
    if !out_file = "" 
    then Filename.chop_extension file ^ (if !only_asm then ".s" else ".exe")
    else !out_file
  in

  (if !only_asm then compile_to_asm else compile_to_exe file) src out_file 
