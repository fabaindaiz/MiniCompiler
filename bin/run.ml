open Dev.Parse
open Dev.Interp
open Dev.Lib
open Printf

let () =
  let args = Sys.argv in
  if Array.length args > 1 && Sys.file_exists args.(1)
  then
    let src = sexp_from_file args.(1) in
    let res = interp [] (parse src) in
    printf "%s\n" (string_of_val res)
  else
    printf "usage: run.exe <filename>\n"
