open Cs164.Driver

let run_and_capture cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ignore (Unix.close_process_in ic));
  String.trim (Buffer.contents buf)

let run_test name source expected =
  try
    compile_to_file source;
    ignore
      (Sys.command
         "as program.s -o program.o && clang runtime.c program.o -o program");
    let result = run_and_capture "./program" in
    if result = expected then Printf.printf "[ PASS ] %s\n" name
    else Printf.printf "[ FAIL ] %s: Expected %s, got %s\n" name expected result
  with e -> Printf.printf "[ ERROR ] %s: %s\n" name (Printexc.to_string e)

let run_suite () =
  let tests =
    [
      ("Integer Constant", "42", "42");
      ("Simple Addition", "(+ 1 2)", "3");
      ("Nested Arithmetic", "(+ (+ 2 3) (- 10 5))", "10");
    ]
  in
  Printf.printf "Starting Tests...\n-------------------\n";
  List.iter (fun (name, src, exp) -> run_test name src exp) tests;
  Printf.printf "-------------------\nDone.\n"

let () = run_suite ()
