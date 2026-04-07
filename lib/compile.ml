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

(*
  Stp: 寄存器加载 / Ldp: 寄存器存储
    - 一次保存/恢复两个寄存器
    - 天然保持16字节栈对齐
    - 比分别使用 Str/Ldr 更高效

  > 当我们的函数有返回值时，我们需要使用 ret 返回到调用者（汇编代码）
  ```
  _1:
    mov X0, X0, #10
    ; 返回到调用者（汇编代码）
    ret

  _entry:
    b _1
    ; 回到 runtime
    ret
  ```

  X30（链接寄存器 LR）:
    存储函数的返回地址
    - 当执行 `Bl label` 时：
      - PC（程序计数器）跳转到 label
      - X30 自动设置为 Bl 下一条指令的地址（返回点）

  X19（堆指针）:
    X19 专门存储堆起始地址（来自C运行时的 malloc)
    - C函数可能修改任何 X0-X18 寄存器（调用者保存）
    - 但 X19-X28 是被调用者保存（callee-saved）
    - 按照约定，如果C函数使用这些寄存器，它必须恢复它们
    - 但我们不信任外部C函数，所以主动保存
*)
let extern_function_bridge name =
  [
    Label ("extern_" ^ name);
    (* 保存LR和另一个寄存器以保持16字节栈对齐 *)
    (* 保存X30(LR)：防止 Bl 指令覆盖返回地址 *)
    (* 保存X19：堆指针，C函数可能修改它 *)
    (* 使用Stp/Ldp保存两个寄存器（16字节） *)
    (* 确保栈指针始终保持16字节对齐 *)
    (*
      1. PreIndex模式：先计算地址，再存储，最后更新sp
      2. 计算目标地址：`Sp - 16`（16字节 = 2个寄存器的空间）
      3. 存储寄存器:
        - [Sp-16] ← X30 的低64位
        - [Sp-8] ← X19 的低64位
      4. 更新Sp：Sp ← Sp - 16
    *)
    Stp (X30, X19, Sp, -16, PreIndex);
    (* 调用C函数 *)
    Bl name;
    (* 恢复寄存器 *)
    (*
      1. PostIndex模式：先存储，再计算地址，最后更新sp
      2. 存储寄存器:
        - [Sp] ← X30 的低64位
        - [Sp+8] ← X19 的低64位
      3. 更新Sp：Sp ← Sp + 16
    *)
    Ldp (X30, X19, Sp, 16, PostIndex);
    (* 返回调用者 *)
    Ret;
  ]

let extern_function name = "extern_" ^ name

let call_extern_function name =
  [
    Stp (X30, X19, Sp, -16, PreIndex);
    Bl name;
    Ldp (X30, X19, Sp, 16, PostIndex);
    Ret;
  ]

let ensure_type reg mask tag =
  [
    (* 保证op value不变，不影响后续 *)
    Mov (Reg X9, Reg reg);
    And (Reg X9, Reg X9, Imm mask);
    Cmp (Reg X9, Imm tag);
    (* (Z flag != 0) 不相等则跳转至 error 标签 *)
    Bne (extern_function "error");
  ]

let ensure_type_is_num reg = ensure_type reg num_tagged.mask num_tagged.tag
let ensure_type_is_bool reg = ensure_type reg bool_tagged.mask bool_tagged.tag
let ensure_type_is_pair reg = ensure_type reg heap_tagged.mask pair_tagged.tag

(* (n + 15) / 16 * 16 *)
let align_to_16 bytes = (bytes + 15) land lnot 15

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

(* function *)
(* (define add (a b) (+ a b)) => defns = [{name = "add"; args = ["a"; "b"]; body = Lst [Sym "+"; Sym "a"; Sym "b"]}] *)
type defn = { name : string; args : string list; body : s_exp }

(* 字符串转换 *)
let string_of_sym = function Sym s -> s | e -> raise (BadExpression e)

(* 解析表达式列表，返回函数定义列表和剩余表达式 *)
(* 限制：
    - 不能先进行逻辑再 define，也不能在 define 之间穿插逻辑表达
    - 强制要求最后一个表达式必须为程序的主体
    - 不支持嵌套定义 *)
let defns_and_body exps =
  let get_defn = function
    (* Lst (Sym name :: args) : 匹配非空列表，第一个内容为name *)
    (* (define (add a b) (+ a b)) => Lst [Sym "define"; Lst [Sym "add"; Sym "x"; Sym "y"]; body] *)
    | Lst [ Sym "define"; Lst (Sym name :: args); body ] ->
        { name; args = List.map string_of_sym args; body }
    | e -> raise (BadExpression e)
  in
  (*
    exps: 剩余表达式列表
    defns：已收集的函数定义列表（累加器）
  *)
  let rec go exps defns =
    match exps with
    (* 只剩一个表达式 *)
    | [ e ] -> (List.rev defns, e)
    (* 至少有一个表达式 *)
    | d :: exps -> go exps (get_defn d :: defns)
    | _ -> raise (BadExpression (Sym "empty"))
  in
  go exps []

(* 遍历列表并查询是否符合条件 *)
let is_defn defns name = List.exists (fun d -> d.name = name) defns

(* 查找符合条件的函数定义 *)
let get_defn defns name = List.find (fun d -> d.name = name) defns

let defn_label s =
  let asm_char c =
    match c with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_' | '$' | '#' | '@' | '~' | '.' | '?' ->
        c
    (* 非法字符替换为下划线 *)
    | _ -> '_'
  in
  (* `Hashtbl.hash` 是非加密哈希 *)
  Printf.sprintf "function_%s_%d" (String.map asm_char s) (Hashtbl.hash s)

let rec compile_exp defns tab stack_index prog =
  match prog with
  | Num n -> [ Mov (Reg X0, operand_of_num n) ]
  | Sym "true" -> [ Mov (Reg X0, operand_of_bool true) ]
  | Sym "false" -> [ Mov (Reg X0, operand_of_bool false) ]
  (* 当监测到形似function名叫f的东西 *)
  | Lst (Sym f :: args) when is_defn defns f ->
      (* 先从符号表里找到function f *)
      let defn = get_defn defns f in
      (* 当args与符号表里找到的defn的args长度一样 *)
      if List.length args = List.length defn.args then
        (* 计算当前函数调用需要的临时栈空间 *)
        (* args 的数量 * 8 字节，并向上对齐到 16 *)
        let arg_space = align_to_16 (List.length args * 8) in
        (* 编译参数并放入临时分配的栈空间了 *)
        let compiled_args =
          args
          |> List.mapi (fun i arg ->
              (* 计算每个参数在临时空间的位置 *)
              let arg_offest = i * 8 in
              (* 递归编译参数，结果在 X0 *)
              compile_exp defns tab stack_index arg
              @ [ Str (X0, BaseOffset (Sp, arg_offest)) ])
          |> List.concat
        in
        (* 组合最终汇编 *)
        [ Sub (Reg Sp, Reg Sp, Imm arg_space) ]
        (* 开辟临时传参空间 *) @ compiled_args (* 执行参数计算和填充 *)
        @ [
            Bl (defn_label f);
            (* 调用函数 (Branch with Link) *)
            Add (Reg Sp, Reg Sp, Imm arg_space) (* 销毁临时传参空间，恢复 SP *);
          ]
      else raise (BadExpression prog)
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; e ] ]; body ] ->
      compile_exp defns tab stack_index e
      (* 从X0存到栈地址 *)
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      (* 把栈地址和变量名放入指表中，继续编译body *)
      @ compile_exp defns
          (Symtab.add var stack_index tab)
          (stack_index - 8) body
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
      let e1_result = compile_exp defns tab stack_index e1 in
      (* 栈上e1的值暂存到栈上，记录值 *)
      let e1_address = [ Str (X0, BaseOffset (Sp, stack_index)) ] in

      (* 计算e2 *)
      let e2_result = compile_exp defns tab (stack_index - 8) e2 in

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
      compile_exp defns tab stack_index e
      (* 类型检查 *)
      @ ensure_type_is_pair X0
      (* 减去tag才能得到真实寻址 *)
      @ [ Ldr (X0, BaseOffset (X0, -pair_tagged.tag)) ]
  | Lst [ Sym "right"; e ] ->
      compile_exp defns tab stack_index e
      (* 类型检查 *)
      @ ensure_type_is_pair X0
      (* 减去tag才能得到真实寻址 *)
      @ [ Ldr (X0, BaseOffset (X0, -pair_tagged.tag + 8)) ]
  | Lst [ Sym "read_num" ] -> [ Bl (extern_function "read_num") ]
  (*
    当遇到一个以 "do" 开头的表达式列表时
     ~ 依次编译列表中的每一个表达式，并将结果合并在一起：

     - `do` 列表的第一个元素应该是 `Sym "do"`
     - `::` 这是列表拼接操作符。
        ~ 这里的意思是“头元素是 `do`，剩下的部分赋值给变量 exps”

    守卫条件（Guard）: `do` 后至少有一个表达式才可以进入此分支

    `List.concat_map` : 对列表中的每个元素应用一个函数，然后将结果合并成一个新列表
  *)
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      List.concat_map (compile_exp defns tab stack_index) exps
  | Lst [ Sym "print"; arg ] ->
      compile_exp defns tab stack_index arg
      @ [
          Bl (extern_function "print_value");
          (* 假占位，应该被扩展成unit *)
          (* 副作用也应该有类型 *)
          Mov (Reg X0, operand_of_bool true);
        ]
  | Lst [ Sym "newline" ] ->
      [
        Bl (extern_function "print_newline"); Mov (Reg X0, operand_of_bool true);
      ]
  | Lst [ Sym "inc"; arg ] ->
      compile_exp defns tab stack_index arg
      (* 类型检查 *)
      @ ensure_type_is_num X0
      @ [ Add (Reg X0, Reg X0, operand_of_num 1) ]
  | Lst [ Sym "dec"; arg ] ->
      compile_exp defns tab stack_index arg
      (* 类型检查 *)
      @ ensure_type_is_num X0
      @ [ Sub (Reg X0, Reg X0, operand_of_num 1) ]
  | Lst [ Sym "not"; arg ] ->
      compile_exp defns tab stack_index arg
      (* 类型检查 *)
      @ ensure_type_is_bool X0
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
      compile_exp defns tab stack_index arg
      (* 类型检查 *)
      @ ensure_type_is_num X0
      @ [ Cmp (Reg X0, operand_of_num 0) ]
      @ zf_to_bool
  | Lst [ Sym "is_num"; arg ] ->
      compile_exp defns tab stack_index arg
      @ [
          (* And : 只有在都是1的情况下才输出1 *)
          And (Reg X0, Reg X0, Imm num_tagged.mask);
          Cmp (Reg X0, Imm num_tagged.tag);
        ]
      @ zf_to_bool
  | Lst [ Sym "if"; test_exp; then_exp; else_exp ] ->
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      compile_exp defns tab stack_index test_exp
      (* test_exp的结果存入X0，使用Cmp验证最终结果是否为false，如果是就跳转else *)
      @ [ Cmp (Reg X0, operand_of_bool false); Beq else_label ]
      (* 如果是true直接运行if内的代码之后跳转continue *)
      @ compile_exp defns tab stack_index then_exp
      @ [ B continue_label ] @ [ Label else_label ]
      @ compile_exp defns tab stack_index else_exp
      @ [ Label continue_label ]
  | Lst [ Sym "+"; e1; e2 ] ->
      compile_exp defns tab stack_index e1
      (* 类型检测 *)
      @ ensure_type_is_num X0
      @ [
          (* Push value to Stack address *)
          Str (X0, BaseOffset (Sp, stack_index));
        ]
        (* Make sure stack index is updated before e2 *)
      @ compile_exp defns tab (stack_index - 8) e2
      (* 类型检测 *)
      @ ensure_type_is_num X0
      (* Arm64与X86不同，设计更为严谨，等价为X0 = X1 + X0 *)
      @ [
          (* Pop value to X1 *)
          Ldr (X1, BaseOffset (Sp, stack_index));
          Add (Reg X0, Reg X1, Reg X0);
        ]
  (* Same like "+" *)
  | Lst [ Sym "-"; e1; e2 ] ->
      compile_exp defns tab stack_index e1
      (* 类型检测 *)
      @ ensure_type_is_num X0
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp defns tab (stack_index - 8) e2
      (* 类型检测 *)
      @ ensure_type_is_num X0
      @ [ Ldr (X1, BaseOffset (Sp, stack_index)); Sub (Reg X0, Reg X1, Reg X0) ]
  | Lst [ Sym "="; e1; e2 ] ->
      compile_exp defns tab stack_index e1
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp defns tab (stack_index - 8) e2
      (* Pop value and comparison *)
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 由于我们会处理至少两个项，此时的e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      (* zf to bool *)
      @ zf_to_bool
  | Lst [ Sym "<"; e1; e2 ] ->
      compile_exp defns tab stack_index e1
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp defns tab (stack_index - 8) e2
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 重要：此时e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      @ lf_to_bool
  | Lst [ Sym ">"; e1; e2 ] ->
      compile_exp defns tab stack_index e1
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp defns tab (stack_index - 8) e2
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 重要：此时e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      @ lf_to_bool
  | e -> raise (BadExpression e)

(* 处理所有函数 *)
(*
  内存布局：

  高地址
  ┌─────────────────┐ ← 调用者的 SP（调用前）
  │ 参数 n          │ ← [sp + stack_size + 8*(n-1)]
  ├─────────────────┤
  │ ...             │
  ├─────────────────┤
  │ 参数 1          │ ← [sp + stack_size]
  ├─────────────────┤ ← sp + stack_size
  │ 未使用空间      │
  ├─────────────────┤ ← sp + 16（local_start）
  │ 局部变量/临时   │
  ├─────────────────┤ ← sp + 8
  │ 保存的 X30 (LR) │
  ├─────────────────┤ ← sp (当前栈顶)
  │ 保存的 X29 (FP) │
  └─────────────────┘ 低地址
*)
let compile_defn defns defn =
  (* 计算函数参数个数, 例如 (define (add a b) ...) 中 a 和 b 两个参数 *)
  let num_args = List.length defn.args in
  (* 栈帧大小计算 *)
  (* 每个参数8字节+ 4个额外槽（用于保存寄存器 X29、X30 等）*)
  (* align_to_16：确保栈大小是16的倍数（ARM64硬件要求栈16字节对齐） *)
  let stack_size = align_to_16 (8 * (num_args + 4)) in
  (* 序言 *)
  let prologue =
    [
      (* 唯一标签 *)
      Label (defn_label defn.name);
      (* 将 X29（帧指针）和 X30（链接寄存器/返回地址）保存到栈中 *)
      (* 将 SP 减去 `stack_size`（分配栈空间） *)
      (* PreIndex` 模式：先计算地址 `sp - stack_size`，存储，然后更新 `sp = sp - stack_size *)
      Stp (X29, X30, Sp, -stack_size, PreIndex);
    ]
  in

  (* 尾声 *)
  let epilogue =
    [
      (* 从 `sp` 位置加载 X29 和 X30 *)
      (* sp = sp + stack_size（释放栈空间）*)
      (* `PostIndex` 模式：先加载，然后更新指针 *)
      Ldp (X29, X30, Sp, stack_size, PostIndex);
      (*返回调用者 （跳转到 X30 中的地址）*)
      Ret;
    ]
  in

  (* 参数映射到正偏移量（在栈帧内部） *)
  (* 参数位于当前栈帧上方（正偏移量），偏移量 = stack_size + 8*i *)
  let ftab =
    defn.args
    |> List.mapi (fun i arg -> (arg, stack_size + (8 * i)))
    (* 返回 (x, 偏移量), ... *)
    |> Symtab.of_list
  in

  (* 局部临时变量起始偏移量，在保存的寄存器之上（X29,X30占用0-16字节） *)
  let local_start = 16 in
  prologue @ compile_exp defns ftab local_start defn.body @ epilogue

(*
  栈向`低地址`增长，所以sp指向栈顶（最低地址）
    - Stp (X29, X30, Sp, -64, PreIndex) 分配64字节栈帧
    - stack_index = 32 作为第一个局部变量的起始位置

  内存地址（从低到高）
  ┌──────────────────────────────┐ ← sp（栈顶，当前执行位置）
  │ 未使用 (0-31字节)            │
  │ sp+0 到 sp+31                │
  ├──────────────────────────────┤ ← sp+32（stack_index起始位置）
  │ 局部变量区域                 │
  │ 例如：[sp, #32]存read_num结果│
  ├──────────────────────────────┤ ← sp+48
  │ 未使用                       │
  ├──────────────────────────────┤ ← sp+56
  │ 保存的 X30 (LR)              │ ← sp+56到sp+63
  ├──────────────────────────────┤ ← sp+64
  │ 保存的 X29 (FP)              │ ← sp+64到sp+71（实际上超出64字节）
  └──────────────────────────────┘

  - [sp, #32] 到 [sp, #16]：局部变量区域 + 空闲空间
  - [sp, #16] 到 [sp, #8]：空闲空间
  - [sp, #8] 到 [sp, #0]：保存的X30和X29
*)
let compile prog =
  (* AppleArm64:
       - 硬件强制规定：任何通过 SP 进行的内存访问, 其 SP 的值必须是 16 字节对齐的
       - 神秘开场祈祷仪式
  *)
  (* 开辟堆空间 *)
  let prologue =
    [
      (* 开辟空间 + 存寄存器 *)
      (* 将 X30 和 X19 存入 [sp-64]，然后将 sp -= 64（PreIndex模式） *)
      Stp (X29, X30, Sp, -64, PreIndex);
      (* !关键!：此时 x19 拿到了真正的堆起始地址 *)
      (* 从 x0 接收 heap 地址并存入 x19 *)
      (* heap 是 entry 函数的第一个量 *)
      Mov (Reg X19, Reg X0);
    ]
  in
  let epilogue =
    [
      (* 恢复寄存器 + 销毁空间 *)
      (* 从 [sp] 读取到 X30 和 X19，然后将 sp += 16（PostIndex模式）*)
      Ldp (X29, X30, Sp, 64, PostIndex);
      Ret;
      (* 返回 C 环境 *)
    ]
  in
  (* defns : 函数，body : 逻辑 *)
  (* 分离 defns 和 body *)
  let defns, body = defns_and_body prog in
  [ Text; Global "entry"; P2align 2 ]
  @ extern_function_bridge "error"
  @ extern_function_bridge "read_num"
  @ extern_function_bridge "print_value"
  @ extern_function_bridge "print_newline"
  @ [ Label "entry" ] @ prologue
  (* 从32字节开始分配，确保在栈帧之内 *)
  @ compile_exp defns Symtab.empty 32 body
  @ epilogue
  @ List.concat_map (compile_defn defns) defns
  |> List.map string_of_directive
  |> String.concat "\n"
