open S_exp
open Asm

exception BadExpression of s_exp

type tagged_layout = { shift : int; mask : int; tag : int }

(*
  数字：后两位是 00（mask = 0b11, tag = 0b00）
  布局：高 62 位存真正的整数值，低 2 位固定为 00
  计算公式：寄存器值 = 整数值 * 4 (即左移 2 位)
*)
let num_tagged : tagged_layout = { shift = 2; mask = 0b11; tag = 0b00 }

(*
  布尔值: 后七位是 0011111（mask = 127, tag = 31
  布局：高位存布尔值（0 或 1），低 7 位是固定死的 0011111
  false：(0 << 7) | 31 = 31 (二进制 0011111)
  true：(1 << 7) | 31 = 159 (二进制 10011111)
*)
let bool_tagged : tagged_layout =
  { shift = 7; mask = 0b1111111; tag = 0b0011111 }

let operand_of_bool b =
  Imm (((if b then 1 else 0) lsl bool_tagged.shift) lor bool_tagged.tag)

let operand_of_num n = Imm ((n lsl num_tagged.shift) lor num_tagged.tag)

(* 零标志位转布尔值 *)
let zf_of_bool =
  [
    Mov (Reg X0, Imm 0);
    (* 如果 刚才的条件 满足 eq (Z (Flag) == 1) 则 X0 = 1  否则 X0 = 0 *)
    Cset (Reg X0, "eq");
    (* 将反转值 0/1 左移 7 位 *)
    Lsl (Reg X0, Reg X0, Imm bool_tagged.shift);
    (* 写入 BoolTag *)
    Orr (Reg X0, Reg X0, Imm bool_tagged.tag);
  ]

let string_of_program prog =
  prog |> List.map string_of_directive |> String.concat "\n"

let rec compile_exp prog =
  match prog with
  | Num n -> [ Mov (Reg X0, operand_of_num n) ]
  | Sym "true" -> [ Mov (Reg X0, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg X0, operand_of_bool false) ]
  | Lst [ Sym "add1"; arg ] ->
      compile_exp arg @ [ Add (Reg X0, Reg X0, operand_of_num 1) ]
  | Lst [ Sym "sub1"; arg ] ->
      compile_exp arg @ [ Sub (Reg X0, Reg X0, operand_of_num 1) ]
  | Lst [ Sym "not"; arg ] ->
      compile_exp arg
      @ [
          (*
            Cmp: X0 - #31 (false)
              同时写入 Flags Z; 定义是：
               Z = 1 ->运算结果为 0
               Z = 0 -> 运算结果不为 0
          *)
          Cmp (Reg X0, operand_of_bool false);
        ]
      @ zf_of_bool
  | Lst [ Sym "is_zero"; arg ] ->
      compile_exp arg @ [ Cmp (Reg X0, operand_of_num 0) ] @ zf_of_bool
  | Lst [ Sym "is_num"; arg ] ->
      compile_exp arg
      @ [
          (* And : 只有在都是1的情况下才输出1 *)
          And (Reg X0, Reg X0, Imm num_tagged.mask);
          Cmp (Reg X0, Imm num_tagged.tag);
        ]
      @ zf_of_bool
  | Lst [ Sym "if"; test_exp; then_exp; else_exp ] ->
      compile_exp test_exp
      (* test_exp的结果存入X0，使用Cmp验证最终结果是否为false，如果是就跳转else *)
      @ [ Cmp (Reg X0, operand_of_bool false); Beq "else" ]
      (* 如果是true直接运行if内的代码之后跳转continue *)
      @ compile_exp then_exp
      @ [ B "continue" ] @ [ Label "else" ] @ compile_exp else_exp
      @ [ Label "continue" ]
  | e -> raise (BadExpression e)

let compile prog =
  string_of_program
    ([ Text; Global "entry"; P2align 2; Label "entry" ]
    @ compile_exp prog @ [ Ret ])
