open S_exp

let is_digit = function '0' .. '9' -> true | _ -> false

let is_symbol_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '+' | '-' | '*' | '/' | '_' | '=' | '<' | '>' ->
      true
  | _ -> false

let is_whitespace = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false
let list_to_string lst = String.of_seq (List.to_seq lst)
let peek seq = match seq () with Seq.Nil -> None | Seq.Cons (h, _) -> Some h

let rec munch cond acc seq =
  match seq () with
  | Seq.Cons (h, ts) when cond h -> munch cond (h :: acc) ts
  | node -> (List.rev acc, fun () -> node)

let consume seq =
  match seq () with Seq.Cons (_, rest) -> rest | Seq.Nil -> seq

let parse_num seq =
  let cs, rest_seq = munch is_digit [] seq in
  let str = list_to_string cs in
  if str = "" then failwith "Expected number"
  else (Num (int_of_string str), rest_seq)

let parse_sym seq =
  let cs, rest_seq = munch is_symbol_char [] seq in
  let str = list_to_string cs in
  (Sym str, rest_seq)

let rec skip_whitespace seq =
  match seq () with
  | Seq.Cons (h, ts) when is_whitespace h -> skip_whitespace ts
  | node -> fun () -> node

let rec parse_sexp seq =
  let seq = skip_whitespace seq in
  match peek seq with
  | None -> (None, seq)
  | Some '(' ->
      let rest = consume seq in
      parse_list [] rest
  | Some ')' -> failwith "Unexpected closing parenthesis"
  | Some c when is_digit c ->
      let num, rest = parse_num seq in
      (Some num, rest)
  | Some c when is_symbol_char c ->
      let sym, rest = parse_sym seq in
      (Some sym, rest)
  | Some c -> failwith (Printf.sprintf "Unknown character: %c" c)

and parse_list acc seq =
  let seq = skip_whitespace seq in
  match peek seq with
  | Some ')' ->
      let rest = consume seq in
      (Some (Lst (List.rev acc)), rest)
  | None -> failwith "Unclosed parenthesis: expected ')'"
  | _ -> (
      match parse_sexp seq with
      | Some s, rest_seq -> parse_list (s :: acc) rest_seq
      | None, _ -> failwith "Unexpected end of input inside list")

let parse src =
  let seq = String.to_seq src in
  let rec loop acc s =
    let s = skip_whitespace s in
    match parse_sexp s with
    | Some sexp, rest -> loop (sexp :: acc) rest
    | None, _ -> List.rev acc
  in
  loop [] seq
