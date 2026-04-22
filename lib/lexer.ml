(*
  <s_exp> ::=
  | NUM(n)
  | SYM(s)
  | LPAREN <lst> RPAREN

  <lst> ::=
  // 空串, 长度为0的字符串, 不包含任何字符
  // 停止递归，列表结束
  | ε
  // 消除左递归，列表在右边
  // 生成一个元素，继续递归
  | <s_exp> <lst>
*)

(* Ast/S_exp *)
open S_exp

(* Token stream *)
type token = NUM of int | SYM of string | LPAREN | RPAREN

exception ParseError

let token_of_string s =
  match s with
  | "(" -> LPAREN
  | ")" -> RPAREN
  | _ -> ( try NUM (int_of_string s) with _ -> SYM s)

let tokenize s =
  let len = String.length s in
  let rec scan i acc =
    if i >= len then List.rev acc
    else
      match s.[i] with
      (* 空白字符：跳过 *)
      | ' ' | '\t' | '\n' | '\r' -> scan (i + 1) acc
      (* 行注释：从 ; 开始到行尾或文件尾 *)
      | ';' ->
          let rec skip_line j =
            if j >= len || s.[j] = '\n' then j else skip_line (j + 1)
          in
          scan (skip_line (i + 1)) acc
      (* 块注释：#| ... |# *)
      | '#' when i + 1 < len && s.[i + 1] = '|' ->
          let rec skip_block j =
            if j >= len then raise ParseError (* 未闭合的 #| *)
            else if s.[j] = '|' && j + 1 < len && s.[j + 1] = '#' then
              j + 2 (* 跳过结束标记 |# *)
            else skip_block (j + 1)
          in
          scan (skip_block (i + 2)) acc
      | '(' -> scan (i + 1) (LPAREN :: acc)
      | ')' -> scan (i + 1) (RPAREN :: acc)
      (* 数字：连续 0-9 *)
      | c when c >= '0' && c <= '9' ->
          let rec read_num j =
            if j < len && s.[j] >= '0' && s.[j] <= '9' then read_num (j + 1)
            else j
          in
          let j = read_num (i + 1) in
          let n = int_of_string (String.sub s i (j - i)) in
          scan j (NUM n :: acc)
      (* 符号：遇到空白、括号、分号或 #| 时停止 *)
      | _ ->
          let rec read_sym j =
            if j >= len then j
            else
              match s.[j] with
              | ' ' | '\t' | '\n' | '\r' | '(' | ')' | ';' -> j
              | '#' when j + 1 < len && s.[j + 1] = '|' -> j
              | _ -> read_sym (j + 1)
          in
          let j = read_sym (i + 1) in
          let sym = String.sub s i (j - i) in
          scan j (SYM sym :: acc)
  in
  scan 0 []

let rec parse_s_exp toks =
  match toks with
  | NUM n :: toks2 -> (Num n, toks2)
  | SYM s :: toks2 -> (Sym s, toks2)
  | LPAREN :: toks2 ->
      let exp3, toks3 = parse_lst toks2 in
      (Lst exp3, toks3)
  | _ -> raise ParseError

and parse_lst toks =
  match toks with
  | RPAREN :: toks2 -> ([], toks2)
  | _ ->
      let exp2, toks2 = parse_s_exp toks in
      let exp3, toks3 = parse_lst toks2 in
      (exp2 :: exp3, toks3)

let parse s =
  let toks = tokenize s in
  let exp, l = parse_s_exp toks in
  if List.length l = 0 then exp else raise ParseError

let rec parse_many toks =
  match toks with
  | [] -> []
  | _ ->
      let exp, toks = parse_s_exp toks in
      exp :: parse_many toks

let parse_program s =
  let toks = tokenize s in
  parse_many toks
