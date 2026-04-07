open Cs164.Driver

let () =
  match Sys.argv with
  | [| _ |] -> repl ()
  | [| _; filename |] ->
      let source = read_file filename in
      compile_to_file source;
      build_and_run ()
  | _ -> print_endline "usage: program [file]"
