open Compile
open Lexer

exception Lexer_error of string

(* with_open_text接受一个匿名函数来指导拿到文件句柄后干什么 *)
let read_file filename = In_channel.with_open_text filename In_channel.input_all

let write_file filename content =
  Out_channel.with_open_text filename (fun oc -> output_string oc content)

let compile_source source =
  let prog = parse source in
  match prog with
  (* [ e ] -> [ e ] : 恰好一个表达式，现在我们需要支持一次识别一个以上表达式了 *)
  | [] -> raise (Lexer_error "empty program")
  | _ -> compile prog

let compile_to_file source =
  let asm = compile_source source in
  write_file "program.s" (asm ^ "\n")

let run_cmd cmd =
  let ic = Unix.open_process_in cmd in
  Fun.protect
    (fun () ->
      try
        while true do
          input_line ic |> print_endline
        done
      with End_of_file -> ())
    ~finally:(fun () -> close_in ic)

let build_and_run () =
  run_cmd
    "as program.s -o program.o && clang runtime.c program.o -o program && \
     ./program"

let rec repl () =
  print_string "> ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> ()
  | source ->
      (try
         compile_to_file source;
         build_and_run ()
       with e -> Printf.printf "Error: %s\n" (Printexc.to_string e));
      repl ()
