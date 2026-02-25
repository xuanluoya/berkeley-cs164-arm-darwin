open Cs164.Lexer
open Cs164.Compile

let () =
  let prog =
    match parse "(is_num false)" with
    | [ e ] -> e
    | _ -> failwith "expected exactly one expression"
  in
  let asm = compile prog in
  let oc = open_out "program.s" in
  Printf.fprintf oc "%s\n" asm;
  close_out oc
