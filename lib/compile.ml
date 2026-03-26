open S_exp
open Asm

exception BadExpression of s_exp
exception Compile_error of string

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

let heap_tagged : tagged_layout = { shift = 0; mask = 0b111; tag = 0 }

(*
  Pair 布局：
  后两位是 010, tag = 0b01）
  高 62 位存两个值的地址，低 2 位固定为 01
*)
let pair_tagged : tagged_layout = { shift = 0; mask = 0; tag = 0b010 }

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

let gensym =
  let counter = ref 0 in
  (* 因为闭包的存在counter会被持久化储存 *)
  fun s ->
    let symbol = Printf.sprintf "%s__%d" s !counter in
    counter := !counter + 1;
    symbol

(* 符号表 *)
module Symtab = Map.Make (struct
  type t = string

  (* 使用Stdlib的比较逻辑覆盖实现 *)
  let compare = compare
end)

(* 'a是泛型参数，代表符号表中存储的值的类型 *)
type 'a symtab = 'a Symtab.t

let rec compile_exp tab stack_index prog =
  match prog with
  | Num n -> [ Mov (Reg X0, operand_of_num n) ]
  | Sym "true" -> [ Mov (Reg X0, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg X0, operand_of_bool false) ]
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; body ] ->
      compile_exp tab stack_index e
      (* 将栈上的值存入X0 *)
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      (* 把栈地址和变量名放入指表中，继续编译body *)
      @ compile_exp (Symtab.add var stack_index tab) (stack_index - 8) body
  | Sym var -> (
      (* 依靠变量名寻找栈地址 *)
      match Symtab.find_opt var tab with
      | Some addr ->
          (* 把栈值拉到X0上 *)
          [ Ldr (X0, BaseOffset (Sp, addr)) ]
      | None -> raise (Compile_error ("Undefined variable: " ^ var)))
  | Lst [ Sym "pair"; e1; e2 ] ->
      (* Pair 的约定：
           - 堆中连续的16个字节
           - 起始位置    （偏移 0）：永远放第一个元素 e1
           - 起始位置 + 8（偏移 8）：永远放第二个元素 e2
           *)
      (* 计算e1 *)
      let e1_result = compile_exp tab stack_index e1 in
      (* 栈上e1的值暂存到栈上，记录值 *)
      let e1_address = [ Str (X0, BaseOffset (Sp, stack_index)) ] in

      (* 计算e2 *)
      let e2_result = compile_exp tab (stack_index - 8) e2 in

      (* 拼装Pair *)
      let pair_logic =
        [
          (* 把e1从栈上加载到临时寄存器 x9 *)
          Ldr (X9, BaseOffset (Sp, stack_index));
          (* 将e1存入堆起始位置[X19 + 0] *)
          Str (X9, BaseOffset (X19, 0));
          (* 将e2存入堆偏移位置[X19 + 8] *)
          Str (X0, BaseOffset (X19, 8));
          (* 将当前堆首地址放入x0作为返回值 *)
          Mov (Reg X0, Reg X19);
          (* 写入TAG *)
          Orr (Reg X0, Reg X0, Imm pair_tagged.tag);
          (* 堆指针向后移动16字节 *)
          Add (Reg X19, Reg X19, Imm 16);
        ]
      in
      e1_result @ e1_address @ e2_result @ pair_logic
  | Lst [ Sym "left"; e ] ->
      compile_exp tab stack_index e
      (* 减去tag才能得到真实寻址 *)
      @ [ Ldr (X0, BaseOffset (X0, -pair_tagged.tag)) ]
  | Lst [ Sym "right"; e ] ->
      compile_exp tab stack_index e
      (* 减去tag才能得到真实寻址 *)
      @ [ Ldr (X0, BaseOffset (X0, -pair_tagged.tag + 8)) ]
  | Lst [ Sym "inc"; arg ] ->
      compile_exp tab stack_index arg
      @ [ Add (Reg X0, Reg X0, operand_of_num 1) ]
  | Lst [ Sym "dec"; arg ] ->
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

let string_of_program prog =
  prog |> List.map string_of_directive |> String.concat "\n"

let compile prog =
  (* AppleArm64:
       - 硬件强制规定：任何通过 SP 进行的内存访问, 其 SP 的值必须是 16 字节对齐的
       - 神秘开场祈祷仪式
  *)
  (* 开辟堆空间 *)
  let prologue =
    [
      (* 开辟空间 + 存寄存器 *)
      (* SP -= 32 并且把寄存器存入新的 SP *)
      (* 先把 sp 减去 32，然后把 x29, x30 存入新的 sp 位置 *)
      Stp (X29, X30, Sp, -32, PreIndex);
      (* !关键!：此时 x19 拿到了真正的堆起始地址 *)
      (* 从 x0 接收 heap 地址并存入 x19 *)
      (* heap 是 entry 函数的第一个量 *)
      Mov (Reg X19, Reg X0);
    ]
  in
  let epilogue =
    [
      (* 恢复寄存器 + 销毁空间 *)
      (* 从当前 sp 读出 x29, x30，然后把 sp 加上 32 *)
      Ldp (X29, X30, Sp, 32, PostIndex);
      Ret;
      (* 返回 C 环境 *)
    ]
  in
  string_of_program
    ([ Text; Global "entry"; P2align 2; Label "entry" ]
    @ prologue
    @ compile_exp Symtab.empty (-8) prog
    @ epilogue)
