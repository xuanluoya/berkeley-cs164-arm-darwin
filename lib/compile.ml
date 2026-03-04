open S_exp
open Asm
open Util

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

(* 相同对比并转换为布尔值 *)
let zf_to_bool =
  [
    Mov (Reg X0, Imm 0);
    (* 如果 刚才的条件 满足 eq (Z (Flag) == 1) 则 X0 = 1  否则 X0 = 0 *)
    Cset (Reg X0, "eq");
    (* 将反转值 0/1 左移 7 位 *)
    Lsl (Reg X0, Reg X0, Imm bool_tagged.shift);
    (* 写入 BoolTag *)
    (* Or => (1, 1, 0) (1, 0, 1) (0, 1, 1) (0, 0, 0) *)
    Orr (Reg X0, Reg X0, Imm bool_tagged.tag);
  ]

(* 小于对比并转换为布尔值 *)
let lf_to_bool =
  [
    Mov (Reg X0, Imm 0);
    Cset (Reg X0, "lt");
    Lsl (Reg X0, Reg X0, Imm bool_tagged.shift);
    Orr (Reg X0, Reg X0, Imm bool_tagged.tag);
  ]

let string_of_program prog =
  prog |> List.map string_of_directive |> String.concat "\n"

let rec compile_exp tab stack_index prog =
  match prog with
  | Num n -> [ Mov (Reg X0, operand_of_num n) ]
  | Sym "true" -> [ Mov (Reg X0, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg X0, operand_of_bool false) ]
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; body ] ->
      compile_exp tab stack_index e
      (* Str 拉出栈address上的值到X0 *)
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      (* 把值放入指表中，继续编译body *)
      @ compile_exp (Symtab.add var stack_index tab) (stack_index - 8) body
  | Sym var when Symtab.mem var tab ->
      (* 寻找值栈位置 *)
      let addr = Symtab.find var tab in
      (* 把栈值拉到X0上 *)
      [ Ldr (X0, BaseOffset (Sp, addr)) ]
  | Lst [ Sym "++"; arg ] ->
      compile_exp tab stack_index arg
      @ [ Add (Reg X0, Reg X0, operand_of_num 1) ]
  | Lst [ Sym "--"; arg ] ->
      compile_exp tab stack_index arg
      @ [ Sub (Reg X0, Reg X0, operand_of_num 1) ]
  | Lst [ Sym "not"; arg ] ->
      compile_exp tab stack_index arg
      @ [
          (*
            Cmp: X0 - #31 (false)
              同时写入 Flags Z; 定义是：
               Z = 1 ->运算结果为 0
               Z = 0 -> 运算结果不为 0
          *)
          Cmp (Reg X0, operand_of_bool false);
        ]
      @ zf_to_bool
  | Lst [ Sym "is_zero"; arg ] ->
      compile_exp tab stack_index arg
      @ [ Cmp (Reg X0, operand_of_num 0) ]
      @ zf_to_bool
  | Lst [ Sym "is_num"; arg ] ->
      compile_exp tab stack_index arg
      @ [
          (* And : 只有在都是1的情况下才输出1 *)
          And (Reg X0, Reg X0, Imm num_tagged.mask);
          Cmp (Reg X0, Imm num_tagged.tag);
        ]
      @ zf_to_bool
  | Lst [ Sym "if"; test_exp; then_exp; else_exp ] ->
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      compile_exp tab stack_index test_exp
      (* test_exp的结果存入X0，使用Cmp验证最终结果是否为false，如果是就跳转else *)
      @ [ Cmp (Reg X0, operand_of_bool false); Beq else_label ]
      (* 如果是true直接运行if内的代码之后跳转continue *)
      @ compile_exp tab stack_index then_exp
      @ [ B continue_label ] @ [ Label else_label ]
      @ compile_exp tab stack_index else_exp
      @ [ Label continue_label ]
  | Lst [ Sym "+"; e1; e2 ] ->
      compile_exp tab stack_index e1
      @ [
          (* Push value to Stack address *)
          Str (X0, BaseOffset (Sp, stack_index));
        ]
        (* Make sure stack index is updated before e2 *)
      @ compile_exp tab (stack_index - 8) e2
      (* Arm64与X86不同，设计更为严谨，等价为X0 = X1 + X0 *)
      @ [
          (* Pop value to X1 *)
          Ldr (X1, BaseOffset (Sp, stack_index));
          Add (Reg X0, Reg X1, Reg X0);
        ]
  (* Same like "+" *)
  | Lst [ Sym "-"; e1; e2 ] ->
      compile_exp tab stack_index e1
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp tab (stack_index - 8) e2
      @ [ Ldr (X1, BaseOffset (Sp, stack_index)); Sub (Reg X0, Reg X1, Reg X0) ]
  | Lst [ Sym "="; e1; e2 ] ->
      compile_exp tab stack_index e1
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp tab (stack_index - 8) e2
      (* Pop value and comparison *)
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 由于我们会处理至少两个项，此时的e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      (* zf to bool *)
      @ zf_to_bool
  | Lst [ Sym "<"; e1; e2 ] ->
      compile_exp tab stack_index e1
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp tab (stack_index - 8) e2
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 重要：此时e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      @ lf_to_bool
  | Lst [ Sym ">"; e1; e2 ] ->
      compile_exp tab stack_index e1
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp tab (stack_index - 8) e2
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 重要：此时e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      @ lf_to_bool
  | e -> raise (BadExpression e)

let compile prog =
  string_of_program
    ([ Text; Global "entry"; P2align 2; Label "entry" ]
    @ compile_exp Symtab.empty (-8) prog
    @ [ Ret ])
