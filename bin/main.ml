open Cs164.Driver

let () =
  match Sys.argv with
  | [| _ |] -> repl ()
  | [| _; filename |] ->
      print_endline "[INFO] Compile start.";
      let source = read_file filename in
      compile_to_file source;
      print_endline "[INFO] Compile done.";
      print_endline "[INFO] Runtime build and run code.";
      build_and_run ();
      print_endline "[INFO] Done."
  | _ -> print_endline "usage: program [file]"
