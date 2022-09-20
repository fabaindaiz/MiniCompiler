open Dev.Parse
open Dev.Interp
open Printf

let () =
  let args = Sys.argv in
  if Array.length args > 1 && Sys.file_exists args.(1)
  then
    let src = sexp_from_file args.(1) in
    let res = interp_prog (parse_prog src) empty_env in
    printf "%s\n" (string_of_val res)
  else
    printf "usage: run_interp.exe <filename>\n"
