type register =
  | X0 (* 返回值 / 第一个参数 *)
  | X1 (* 第二个参数 *)
  | X19 (* callee-saved *)
  | Sp (* 栈指针 *)
  | Fp (* 帧指针 *)
  | Lr (* 返回地址 *)

let string_of_register = function
  | X0 -> "x0"
  | X1 -> "x1"
  | X19 -> "x19"
  | Sp -> "sp"
  | Fp -> "x29"
  | Lr -> "x30"

type operand = Reg of register | Imm of int
type address = BaseOffset of register * int | BaseIndex of register * register
type addressing = Offset | PreIndex | PostIndex

(* Apple Arm64 指令集（子集，包含所有需要用到的） *)
type directive =
  | Text (* 段切换：生成 `.text`，表示接下来是代码段（instruction section） *)
  | Global of string (* 生成 `.global _l`, 把 _l 这个符号导出给链接器，l 可以换成任何名字 *)
  | Label of string (* 定义一个符号，代表“当前地址” *)
  | P2align of int (* 让接下来的 label 按 2² = 4 字节对齐，ARM64 指令长度是 4 字节固定 *)
  | Mov of operand * operand
  | Ldr of register * address (* 将数据从 内存地址 加载到 寄存器 中 *)
  | Str of register * address (* 将数据从 寄存器 存储到 内存地址 中 *)
  | Adrp of register * string
  | AddLabel of register * register * string
  | Stp of register * register * register * int * addressing
  | Ldp of register * register * register * int * addressing
  | Add of operand * operand * operand
  | Sub of operand * operand * operand
  | And of operand * operand * operand
  | Orr of operand * operand * operand
  | Lsl of operand * operand * operand
  | Lsr of operand * operand * operand
  | Cmp of operand * operand
  | Cset of operand * string
  | B of string
  | Br of register
  | Beq of string
  | Bne of string
  | Bl of string
  | Blr of register
  | Ret
  | Comment of string

let string_of_address = function
  (* 栈寻址，基于偏移量 *)
  | BaseOffset (rn, off) ->
      Printf.sprintf "[%s, #%d]" (string_of_register rn) off
  (* 栈寻址，基于寄存器 *)
  | BaseIndex (rn, rm) ->
      Printf.sprintf "[%s, %s]" (string_of_register rn) (string_of_register rm)

(* let string_of_base_offset base offset =
  Printf.sprintf "[%s, #%d]" (string_of_register base) offset

let string_of_stp_ldp mnemonic r1 r2 base offset mode =
  let regs =
    Printf.sprintf "%s, %s" (string_of_register r1) (string_of_register r2)
  in
  match mode with
  | Offset ->
      Printf.sprintf "\t%s %s, %s" mnemonic regs
        (string_of_base_offset base offset)
  | PreIndex ->
      Printf.sprintf "\t%s %s, [%s, #%d]!" mnemonic regs
        (string_of_register base) offset
  | PostIndex ->
      Printf.sprintf "\t%s %s, [%s], #%d" mnemonic regs
        (string_of_register base) offset *)

let string_of_stp_ldp mnemonic r1 r2 base offset mode =
  let regs =
    Printf.sprintf "%s, %s" (string_of_register r1) (string_of_register r2)
  in
  match mode with
  | Offset ->
      let addr_str = string_of_address (BaseOffset (base, offset)) in
      Printf.sprintf "\t%s %s, %s" mnemonic regs addr_str
  | PreIndex ->
      Printf.sprintf "\t%s %s, [%s, #%d]!" mnemonic regs
        (string_of_register base) offset
  | PostIndex ->
      Printf.sprintf "\t%s %s, [%s], #%d" mnemonic regs
        (string_of_register base) offset

let string_of_directive = function
  | Text -> ".text"
  | Global l -> Printf.sprintf ".global _%s" l
  | P2align i -> Printf.sprintf ".p2align %d" i
  | Label l -> Printf.sprintf "_%s:" l
  | Adrp (rd, label) ->
      Printf.sprintf "\tadrp %s, _%s@PAGE" (string_of_register rd) label
  | AddLabel (rd, rn, label) ->
      Printf.sprintf "\tadd %s, %s, _%s@PAGEOFF" (string_of_register rd)
        (string_of_register rn) label
  | Stp (r1, r2, base, offset, mode) ->
      string_of_stp_ldp "stp" r1 r2 base offset mode
  | Ldp (r1, r2, base, offset, mode) ->
      string_of_stp_ldp "ldp" r1 r2 base offset mode
  | Mov (Reg rd, Reg rn) ->
      Printf.sprintf "\tmov %s, %s" (string_of_register rd)
        (string_of_register rn)
  | Mov (Reg rd, Imm i) ->
      Printf.sprintf "\tmov %s, #%d" (string_of_register rd) i
  | Ldr (rd, addr) ->
      Printf.sprintf "\tldr %s, %s" (string_of_register rd)
        (string_of_address addr)
  | Str (rs, addr) ->
      Printf.sprintf "\tstr %s, %s" (string_of_register rs)
        (string_of_address addr)
  | Add (Reg rd, Reg rn, Reg rm) ->
      Printf.sprintf "\tadd %s, %s, %s" (string_of_register rd)
        (string_of_register rn) (string_of_register rm)
  | Add (Reg rd, Reg rn, Imm i) ->
      Printf.sprintf "\tadd %s, %s, #%d" (string_of_register rd)
        (string_of_register rn) i
  | Sub (Reg rd, Reg rn, Reg rm) ->
      Printf.sprintf "\tsub %s, %s, %s" (string_of_register rd)
        (string_of_register rn) (string_of_register rm)
  | Sub (Reg rd, Reg rn, Imm i) ->
      Printf.sprintf "\tsub %s, %s, #%d" (string_of_register rd)
        (string_of_register rn) i
  | And (Reg rd, Reg rn, Reg rm) ->
      Printf.sprintf "\tand %s, %s, %s" (string_of_register rd)
        (string_of_register rn) (string_of_register rm)
  | And (Reg rd, Reg rn, Imm imm) ->
      Printf.sprintf "\tand %s, %s, #%d" (string_of_register rd)
        (string_of_register rn) imm
  | Orr (Reg rd, Reg rn, Reg rm) ->
      Printf.sprintf "\torr %s, %s, %s" (string_of_register rd)
        (string_of_register rn) (string_of_register rm)
  | Orr (Reg rd, Reg rn, Imm imm) ->
      Printf.sprintf "\torr %s, %s, #%d" (string_of_register rd)
        (string_of_register rn) imm
  | Lsl (Reg rd, Reg rn, Imm i) ->
      Printf.sprintf "\tlsl %s, %s, #%d" (string_of_register rd)
        (string_of_register rn) i
  | Lsr (Reg rd, Reg rn, Imm i) ->
      Printf.sprintf "\tlsr %s, %s, #%d" (string_of_register rd)
        (string_of_register rn) i
  | Cmp (Reg rn, Reg rm) ->
      Printf.sprintf "\tcmp %s, %s" (string_of_register rn)
        (string_of_register rm)
  | Cmp (Reg rn, Imm i) ->
      Printf.sprintf "\tcmp %s, #%d" (string_of_register rn) i
  | Cset (Reg rd, cond) ->
      Printf.sprintf "\tcset %s, %s" (string_of_register rd) cond
  | B l -> Printf.sprintf "\tb _%s" l
  | Beq l -> Printf.sprintf "\tb.eq _%s" l
  | Bl l -> Printf.sprintf "\tbl _%s" l
  | Br rn -> Printf.sprintf "\tbr %s" (string_of_register rn)
  | Blr rn -> Printf.sprintf "\tblr %s" (string_of_register rn)
  | Ret -> "\tret"
  | Comment s -> "\t; " ^ s
  | _ -> "; Instruction not yet fully implemented"
