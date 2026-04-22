open S_exp
open Ast
open Asm

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

(* 字符串转换 *)
let string_of_sym = function Sym s -> s | e -> raise (BadSExpression e)

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

(*
  defns           => 所有已定义的函数列表
  tab             => 变量→栈偏移量的符号表
  stack_index     => 下一个可用的局部变量槽位
  prog            => 当前要编译的表达式
  is_tail         => 是否处于"尾位置"
  curr_stack_size => 当前函数的栈帧大小
*)
let rec compile_exp ?(curr_stack_size = 0) defns tab stack_index prog is_tail =
  match prog with
  | Num n -> [ Mov (Reg X0, operand_of_num n) ]
  | True -> [ Mov (Reg X0, operand_of_bool true) ]
  | False -> [ Mov (Reg X0, operand_of_bool false) ]
  (* 当监测到形似function名叫f的东西,并且不是尾递归 *)
  | Call (f, args) when is_defn defns f && not is_tail ->
      (* 先从符号表里找到function f *)
      let defn = get_defn defns f in
      (* 当args与符号表里找到的defn的args长度一样 *)
      (* 为什么？难道你定义了 (add a b) 调用时要传如 (add 1 2 3) 吗？栈会爆炸 *)
      if List.length args = List.length defn.args then
        (* 计算当前函数调用需要的临时栈空间 *)
        (* args 的数量 * 8 字节，并向上对齐到 16 *)
        (* arg_space（总量）：它是我们要挪动 SP 的总距离
          - 为了保证移动后的 SP 依然合法（对齐），这个距离必须是 16 的倍数 *)
        let arg_space = align_to_16 (List.length args * 8) in
        (* 编译参数并放入临时分配的栈空间了 *)
        let compiled_args =
          (* list *)
          args
          |> List.mapi (fun i arg ->
              (* 计算每个参数在临时空间的位置 *)
              (* arg_offset（内部偏移）：一旦 SP 对齐了（我们已经对齐了），我们在这一片“合法领地”内部划分小格子
                - 每个参数（64位数据）占 8 字节
                - 在内部，我们可以自由访问 [Sp, #0], [Sp, #8], [Sp, #16]
                - 只要基地址 SP 是对齐的，这些 8 字节步进的偏移量是完全合法的
                - (- arg_space) 是因为我们要把值提前放进之后会使用Sub囊括的内存里 *)
              (*
               *)
              let arg_offest = (i * 8) - arg_space in
              (* 递归编译参数，结果在 X0 *)
              compile_exp ~curr_stack_size defns tab stack_index arg false
              @
              (* compile_exp的结果会存到X0，我们转存到它该呆的地方 *)
              [ Str (X0, BaseOffset (Sp, arg_offest)) ])
          |> List.concat
        in
        (* 组合最终汇编 *)
        (* 1. 先计算所有参数并存在当前函数的局部变量区 *)
        compiled_args
        @ [
            (* 开空间 *)
            (* 2. 全部算完后，一次性把它们从局部变量区“搬”到参数区 *)
            (* 在调用者的栈帧顶部创建了一个临时缓冲 *)
            (* 开辟临时传参空间，函数默认寻找args的地方就是栈上面 *)
            (*
            !! 被调用函数通过`固定的偏移量`访问参数 !!
            参数传递：参数在内存中的`绝对地址没有变化`，只是相对于当前SP的偏移增加了 `stack_size`
            其实根本没有发生“物理传输”，发生的是“视角切换”:
              1. (调用者视角)：我把参数存入 [Sp, #0], [Sp, #8]，然后我保持 SP 不动，执行 Bl function
              2. (跳转)：程序跳进函数内部。
              3. (被调用者视角)：函数第一件事是执行 Stp X29, X30, [Sp, -stack_size]!
                  !! 函数把 SP 又向下移动了 !!
              4. 结果：现在，原来调用者存的参数，相对于函数现在新的 SP，
                  - 偏移量就变成了 current_SP + stack_size + 8*i
                  - 我们只是补上了Sp下移的距离，参数本身并没有在物理空间内发生变化
            *)
            Sub (Reg Sp, Reg Sp, Imm arg_space);
            (*
            使用 Bl（Branch with Link）调用时，CPU会自动把返回地址写入 X30（链接寄存器 LR），然后跳转到函数
            函数序言（prologue）又会把调用者的 X29/X30 保存到栈上，开辟新栈帧
            *)
            Bl (defn_label f);
            (* 调用函数 (Branch with Link) *)
            Add (Reg Sp, Reg Sp, Imm arg_space) (* 销毁临时传参空间，恢复 SP *);
          ]
      else raise (BadExpression prog)
  (* 优化尾递归调用 *)
  (*
    一般函数的栈布局 => 调用链是 A → B → C → D 时，每次调用都会建立一个新的栈帧
    高地址
    ├─────────────────────────┤
    │  参数区                 │
    ├─────────────────────────┤ ← A 调用 B 前的 SP (Old SP)
    │  X29 (FP)               │ ← B 的栈帧
    │  X30 (LR=A的返回地址)   │
    │  局部变量               │
    ├─────────────────────────┤ ← B 调用 C 前的 SP
    │  X29 (FP)               │ ← C 的栈帧
    │  X30 (LR=B的返回地址)   │
    │  局部变量               │
    ├─────────────────────────┤ ← C 调用 D 前的 SP
    │  X29 (FP)               │ ← D 的栈帧
    │  X30 (LR=C的返回地址)   │
    │  局部变量               │
    ├─────────────────────────┤ ← 当前 SP (不断下降)
    低地址
    每一层调用，X30 都保存了上一层的返回地址，形成一条链。返回时逐层 ret，栈空间线性增长

    例如：
    ```ocaml
    (define (sum n total)
      (if (zero? n)
          total
          (sum (dec n) (+ n total))))  ; ← 这是最后一个操作！
    ```
    调用者 → sum(1000000, 0) → sum(999999, 1000000) → sum(999998, 1999999) → ...
    每一层都要在栈上保存一帧（48字节），100万次调用 ≈ 48MB 栈空间，远超系统默认栈大小（通常8MB），触发栈溢出（Segmentation Fault）

    是尾递归调用：
    (sum ...) 的返回值就是整个 if 的返回值，也就是整个函数的返回值
    我们不需要回到 if 这里再继续做什么——直接返回给 sum 的调用者就行

    - 当前栈帧里的局部变量、保存的寄存器，全部没用了
    - 可以直接复用当前栈帧，直接覆盖参数后重新执行函数
  *)
  (* 是尾递归 (is_defn -> true = is_tail -> true) *)
  | Call (f, args) when is_defn defns f && is_tail ->
      let defn = get_defn defns f in
      if List.length args = List.length defn.args then
        let compiled_args =
          (* 从后往前计算参数，直接覆盖参数区
             为什么从后往前计算？
              - 因为计算后面的参数可能依赖前面参数的旧值 *)
          args |> List.rev
          |> List.mapi (fun i arg ->
              (* 从后往前索引 *)
              let param_idx = List.length args - 1 - i in
              (* 在栈上的位置 *)
              let param_offset = curr_stack_size + (param_idx * 8) in
              compile_exp ~curr_stack_size defns tab stack_index arg false
              (* 新参数写入栈 *)
              @ [ Str (X0, BaseOffset (Sp, param_offset)) ])
          |> List.concat
        in
        compiled_args
        @ [
            (* 恢复当前栈帧，回到调用者视角 *)
            Ldp (X29, X30, Sp, curr_stack_size, PostIndex);
            (* 直接跳转复用栈帧，不保存新返回地址 *)
            B (defn_label f);
          ]
      else raise (BadExpression prog)
  | Call _ -> raise (BadExpression prog)
  | Let (var, e, body) ->
      compile_exp ~curr_stack_size defns tab stack_index e false
      (* 从X0存到栈地址 *)
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      (* 把栈地址和变量名放入指表中，继续编译body *)
      @ compile_exp ~curr_stack_size defns
          (Symtab.add var stack_index tab)
          (stack_index - 8) body is_tail
  | Var var -> (
      (* 依靠变量名寻找栈地址 *)
      match Symtab.find_opt var tab with
      | Some addr ->
          (* 把栈值拉到X0上 *)
          [ Ldr (X0, BaseOffset (Sp, addr)) ]
      | None -> raise (Compile_error ("Undefined variable: " ^ var)))
  | Prim2 (Cons, e1, e2) ->
      (* Pair 的约定：
           - 堆中连续的16个字节
           - 起始位置    （偏移 0）：永远放第一个元素 e1
           - 起始位置 + 8（偏移 8）：永远放第二个元素 e2
           *)
      (* 计算e1 *)
      let e1_result =
        compile_exp ~curr_stack_size defns tab stack_index e1 false
      in
      (* 栈上e1的值暂存到栈上，记录值 *)
      let e1_address = [ Str (X0, BaseOffset (Sp, stack_index)) ] in

      (* 计算e2 *)
      let e2_result =
        compile_exp ~curr_stack_size defns tab (stack_index - 8) e2 false
      in

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
  | Prim1 (Car, e) ->
      compile_exp ~curr_stack_size defns tab stack_index e false
      (* 类型检查 *)
      @ ensure_type_is_pair X0
      (* 减去tag才能得到真实寻址 *)
      @ [ Ldr (X0, BaseOffset (X0, -pair_tagged.tag)) ]
  | Prim1 (Cdr, e) ->
      compile_exp ~curr_stack_size defns tab stack_index e false
      (* 类型检查 *)
      @ ensure_type_is_pair X0
      (* 减去tag才能得到真实寻址 *)
      @ [ Ldr (X0, BaseOffset (X0, -pair_tagged.tag + 8)) ]
  | Prim0 ReadNum -> [ Bl (extern_function "read_num") ]
  (*
    当遇到一个以 "do" 开头的表达式列表时
     ~ 依次编译列表中的每一个表达式，并将结果合并在一起：

     - `do` 列表的第一个元素应该是 `Sym "do"`
     - `::` 这是列表拼接操作符。
        ~ 这里的意思是“头元素是 `do`，剩下的部分赋值给变量 exps”

    守卫条件（Guard）: `do` 后至少有一个表达式才可以进入此分支

    `List.concat_map` : 对列表中的每个元素应用一个函数，然后将结果合并成一个新列表
  *)
  | Do exps ->
      List.mapi
        (fun i exp ->
          compile_exp ~curr_stack_size defns tab stack_index exp
            (*
            true	强制标记为尾位置	函数体最顶层、经过尾递归转换后的循环体
            false	强制标记为非尾位置	函数参数、运算符的子表达式、do 前面的表达式
            is_tail	继承外层的尾位置状态	if 的分支、let 的 body、do 最后一个表达式
           *)
            (if i = List.length exps - 1 then is_tail else false))
        exps
      |> List.concat
  | Prim1 (Display, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      @ [
          Bl (extern_function "print_value");
          (* 假占位，应该被扩展成unit *)
          (* 副作用也应该有类型 *)
          Mov (Reg X0, operand_of_bool true);
        ]
  | Prim0 NewLine ->
      [
        Bl (extern_function "print_newline"); Mov (Reg X0, operand_of_bool true);
      ]
  | Prim1 (Add1, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      (* 类型检查 *)
      @ ensure_type_is_num X0
      @ [ Add (Reg X0, Reg X0, operand_of_num 1) ]
  | Prim1 (Sub1, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      (* 类型检查 *)
      @ ensure_type_is_num X0
      @ [ Sub (Reg X0, Reg X0, operand_of_num 1) ]
  | Prim1 (Not, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
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
  | Prim1 (ZeroP, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      (* 类型检查 *)
      @ ensure_type_is_num X0
      @ [ Cmp (Reg X0, operand_of_num 0) ]
      @ zf_to_bool
  | Prim1 (NumP, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      @ [
          (* And : 只有在都是1的情况下才输出1 *)
          And (Reg X0, Reg X0, Imm num_tagged.mask);
          Cmp (Reg X0, Imm num_tagged.tag);
        ]
      @ zf_to_bool
  | If (test_exp, then_exp, else_exp) ->
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      compile_exp ~curr_stack_size defns tab stack_index test_exp false
      (* test_exp的结果存入X0，使用Cmp验证最终结果是否为false，如果是就跳转else *)
      @ [ Cmp (Reg X0, operand_of_bool false); Beq else_label ]
      (* 如果是true直接运行if内的代码之后跳转continue *)
      @ compile_exp ~curr_stack_size defns tab stack_index then_exp is_tail
      @ [ B continue_label ] @ [ Label else_label ]
      @ compile_exp ~curr_stack_size defns tab stack_index else_exp is_tail
      @ [ Label continue_label ]
  | Prim2 (Plus, e1, e2) ->
      compile_exp ~curr_stack_size defns tab stack_index e1 false
      (* 类型检测 *)
      @ ensure_type_is_num X0
      @ [
          (* Push value to Stack address *)
          Str (X0, BaseOffset (Sp, stack_index));
        ]
        (* Make sure stack index is updated before e2 *)
      @ compile_exp ~curr_stack_size defns tab (stack_index - 8) e2 false
      (* 类型检测 *)
      @ ensure_type_is_num X0
      (* Arm64与X86不同，设计更为严谨，等价为X0 = X1 + X0 *)
      @ [
          (* Pop value to X1 *)
          Ldr (X1, BaseOffset (Sp, stack_index));
          Add (Reg X0, Reg X1, Reg X0);
        ]
  (* Same like "+" *)
  | Prim2 (Minus, e1, e2) ->
      compile_exp ~curr_stack_size defns tab stack_index e1 false
      (* 类型检测 *)
      @ ensure_type_is_num X0
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp ~curr_stack_size defns tab (stack_index - 8) e2 false
      (* 类型检测 *)
      @ ensure_type_is_num X0
      @ [ Ldr (X1, BaseOffset (Sp, stack_index)); Sub (Reg X0, Reg X1, Reg X0) ]
  | Prim2 (Eq, e1, e2) ->
      compile_exp ~curr_stack_size defns tab stack_index e1 false
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp ~curr_stack_size defns tab (stack_index - 8) e2 false
      (* Pop value and comparison *)
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 由于我们会处理至少两个项，此时的e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      (* zf to bool *)
      @ zf_to_bool
  | Prim2 (Gt, e1, e2) ->
      compile_exp ~curr_stack_size defns tab stack_index e1 false
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp ~curr_stack_size defns tab (stack_index - 8) e2 false
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 重要：此时e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      @ lf_to_bool
  | Prim2 (Lt, e1, e2) ->
      compile_exp ~curr_stack_size defns tab stack_index e1 false
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp ~curr_stack_size defns tab (stack_index - 8) e2 false
      @ [
          Ldr (X1, BaseOffset (Sp, stack_index));
          (* 重要：此时e1被存放在X1 *)
          Cmp (Reg X1, Reg X0);
        ]
      @ lf_to_bool

(* 处理所有函数 - 下面有内存布局 *)

let compile_defn defns defn =
  (* 计算函数参数个数, 例如 (define (add a b) ...) 中 a 和 b 两个参数 *)
  let num_args = List.length defn.args in
  (* 栈帧大小计算 *)
  (* 每个参数 8字节 + 4个额外槽（用于保存寄存器 X29、X30 等）*)
  (* align_to_16：确保栈大小是16的倍数（ARM64硬件要求栈16字节对齐） *)
  let stack_size = align_to_16 (8 * (num_args + 4)) in
  (* 序言 *)
  let prologue =
    [
      (* 唯一标签 *)
      Label (defn_label defn.name);
      (* 开辟空间：为当前函数申请 stack_size 大小的局部变量空间 *)
      (* Tips: X30 被称为链接寄存器 (Link Register, LR)，它的唯一任务是：记录函数执行完后应该跳回到哪
          - 后果：如果没有在调用前保存 X30，当函数 A 执行到最后的 Ret 指令时，
          - 它会试图跳回到 X30 指向的位置。由于 X30 已经被修改，
          - 程序将无法回到函数 A 的调用者，而是陷入死循环或者崩溃。*)
      (* Tips: X29 - 建立回溯链：
          - 保存旧的 X29 并将当前的栈顶（SP）存入 X29，可以形成一个“栈帧链”
          - 这让调试器（如 LLDB）或错误报告工具能够通过追踪 X29
          - 找到每一层函数的调用关系（Stack Walk） *)
      (* 保存现场：把调用者的 X29（旧帧指针）和 X30（返回地址）存起来，以便函数返回时能找回家 *)
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
  (* 参见上文，我们在函数主体中调用函数时，开辟了args*8个字节的空间
     本着不浪费的原则，我们直接去找他们就好了，不要复制到自己的空间
     于是乎，我们计算了他们的坐标离我们多远，由于Stp命令开辟了 -stack_size
     大小的空间，我们当前的 Sp 指针自然需要加上 stack_size，而我们还知道
     每个参数占用八字节，自然就是需要 i * 8
  *)
  let ftab =
    defn.args
    (* mapi index begin from 0 *)
    |> List.mapi (fun i arg -> (arg, stack_size + (8 * i)))
    (* 返回 (x, 偏移量), ... *)
    |> Symtab.of_list
  in

  (* 局部临时变量起始偏移量，在保存的寄存器之上（X29,X30占用0-16字节） *)
  let local_start = 16 in
  prologue
  (* curr_stack_size => 当前函数栈帧的大小 *)
  (* 函数体的返回值就是整个函数的返回值，没有任何外层包装，绝对是尾位置 *)
  @ compile_exp ~curr_stack_size:stack_size defns ftab local_start defn.body
      true
  @ epilogue

(*
  堆空间 (Heap Layout) - 由 X19 指向
    - 堆向高地址增长 每次分配 Pair 占用 16 字节（8 字节 Left + 8 字节 Right）

  低地址
  ├──────────────────────────────┤
  │ ... 已分配的数据 ...         │
  ├──────────────────────────────┤ <--- X19 (当前分配起始点)
  │ Element 1 (Left)  (8 bytes)  │ [X19, #0]
  │ Element 2 (Right) (8 bytes)  │ [X19, #8]
  ├──────────────────────────────┤ <--- X19 + 16 (下次分配点)
  │ ... 未分配空间 ...           │
  高地址

  栈帧结构 (Stack Frame) - 向低地址增长，SP 必须 16 字节对齐
    - 每个函数调用通过 `Stp X29, X30, [Sp, -stack_size]!` 开辟空间

  高地址 (调用者方向)
  ├──────────────────────────────┤
  │ 参数 n                       │ [Sp, #stack_size + 8*(n-1)]
  │ ...                          │
  │ 参数 1                       │ [Sp, #stack_size]
  ├──────────────────────────────┤ <--- 进入函数前的 SP (Old SP)
  │                              │
  │     --- 当前函数栈帧 ---     │
  │ (大小由 align_to_16 决定)    │
  │                              │
  ├──────────────────────────────┤
  │ 局部变量 / 临时计算空间      │ [Sp, #16] 开始 (local_start)
  ├──────────────────────────────┤
  │ 保存的 X30 (LR - 返回地址)   │ [Sp, #8]
  ├──────────────────────────────┤
  │ 保存的 X29 (FP - 帧指针)     │ [Sp, #0]
  └──────────────────────────────┘ <--- 当前 SP (栈顶)
  低地址

  主入口点 (Entry) 布局
    - Entry 固定分配 64 字节空间，用于存储基础寄存器并为初次编译预留局部空间。
      - [Sp, #0]  : 保存的 X29
      - [Sp, #8]  : 保存的 X30 (LR)
      - [Sp, #32] : 局部变量分配起始点 (stack_index)

            (高地址)
  SP + 64 ┌─────────────────┐
          │ 未使用          │ ← [SP + 48] ~ [SP + 63] (16字节)
  SP + 48 ├─────────────────┤
          │ 临时值1 (e1)    │ ← [SP + 32] ~ [SP + 39] (stack_index=32)
  SP + 40 ├─────────────────┤
          │ 临时值2         │ ← [SP + 24] ~ [SP + 31] (stack_index=24)
  SP + 32 ├─────────────────┤
          │ 临时值3         │ ← [SP + 16] ~ [SP + 23] (stack_index=16)
  SP + 24 ├─────────────────┤
          │ 对齐填充        │ ← [SP + 8] ~ [SP + 15] (X30上方)
  SP + 16 ├─────────────────┤
          │ 保存的 X30 (LR) │ ← [SP + 8] ~ [SP + 15]
  SP + 8  ├─────────────────┤
          │ 保存的 X29 (FP) │ ← [SP + 0] ~ [SP + 7]
  SP + 0  └─────────────────┘
            (低地址)
*)
let compile program =
  (* AppleArm64:
       - 硬件强制规定：任何通过 SP 进行的内存访问, 其 SP 的值必须是 16 字节对齐的
       - 神秘开场祈祷仪式
  *)
  (* 开辟栈空间 *)
  let prologue =
    [
      (* 开辟空间 + 存寄存器 *)
      (* 将 X30 和 X29 存入 [sp-64]，然后将 sp -= 64（PreIndex模式） *)
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
  let prog = program_of_s_exps program in
  [ Text; Global "entry"; P2align 2 ]
  @ extern_function_bridge "error"
  @ extern_function_bridge "read_num"
  @ extern_function_bridge "print_value"
  @ extern_function_bridge "print_newline"
  @ [ Label "entry" ] @ prologue
  (* 从32字节开始分配，确保在栈帧之内 *)
  (* !! `stack_index` 是当前可用栈槽的偏移量 !!
      - 第一个可用槽在 `[SP+32]`
      - 使用后递减 8：下一个可用槽在 `[SP+24]`
      - 继续递减：`[SP+16]` → `[SP+8]` → ... *)
  @ compile_exp ~curr_stack_size:64 prog.defns Symtab.empty 32 prog.body true
  @ epilogue
  @ List.concat_map (compile_defn prog.defns) prog.defns
  |> List.map string_of_directive
  |> String.concat "\n"
