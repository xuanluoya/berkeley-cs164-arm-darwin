open Ast
open Asm

exception Compile_error of string

(*
  shift : 真实数据要左移多少位
  mask  : 用于提取那几位的掩码
  tag   : 类型标签, 占最低几位
*)
type tagged_layout = { shift : int; mask : int; tag : int }

let num_tagged : tagged_layout = { shift = 2; mask = 0b11; tag = 0b00 }

let bool_tagged : tagged_layout =
  { shift = 7; mask = 0b1111111; tag = 0b0011111 }

(*
  所有堆分配的地址都是至少 8 字节或 16 字节对齐的
  这意味着它们的天然最低 3 位就是 000
  我们可以安全地把这 3 位替换为 010 或 110 而不会破坏寻址
  访问时只需把地址减去 tag 值，就能恢复原始对齐地址
*)
let heap_tagged : tagged_layout = { shift = 0; mask = 0b111; tag = 0 }
let pair_tagged : tagged_layout = { shift = 0; mask = 0; tag = 0b010 }
let func_tagged : tagged_layout = { shift = 0; mask = 0; tag = 0b110 }

let operand_of_bool b =
  Imm (((if b then 1 else 0) lsl bool_tagged.shift) lor bool_tagged.tag)

let operand_of_num n = Imm ((n lsl num_tagged.shift) lor num_tagged.tag)

let cond_to_bool cond_str =
  [
    (* X0 = 0 *)
    Mov (Reg X0, Imm 0);
    (* 若条件成立，X0 = 1，否则 0 *)
    Cset (Reg X0, cond_str);
    (* 写入 Bool *)
    Lsl (Reg X0, Reg X0, Imm bool_tagged.shift);
    Orr (Reg X0, Reg X0, Imm bool_tagged.tag);
  ]

let extern_function_bridge name =
  [
    Label ("extern_" ^ name);
    Stp (X30, X19, Sp, -16, PreIndex);
    Bl name;
    Ldp (X30, X19, Sp, 16, PostIndex);
    Ret;
  ]

let extern_function name = "extern_" ^ name

let ensure_type reg mask tag =
  [
    (* 复制值，避免破坏原寄存器 *)
    Mov (Reg X9, Reg reg);
    (* 提取标签位 *)
    And (Reg X9, Reg X9, Imm mask);
    (* 与期望标签比较 -- 正常应该 Z = 1 *)
    (* 用 X9 减去 tag，把结果丢弃，但保留减法产生的状态标志 *)
    Cmp (Reg X9, Imm tag);
    (* 不匹配则报错 *)
    Bne (extern_function "error");
  ]

let ensure_type_is_num reg = ensure_type reg num_tagged.mask num_tagged.tag
let ensure_type_is_pair reg = ensure_type reg heap_tagged.mask pair_tagged.tag
let ensure_type_is_func reg = ensure_type reg heap_tagged.mask func_tagged.tag

(* 栈对齐辅助 -- 将任意字节数向上取整到最近的 16 的倍数 *)
let align_to_16 bytes = (bytes + 15) land lnot 15

(*
  ~curr_stack_size : 当前函数已分配的栈帧总大小（用于尾调用时计算参数区）
  defns            : 程序中所有全局函数定义的列表
  tab              : 符号表（Symtab），变量名 -> 栈槽偏移（相对 SP 的字节偏移）
  stack_index      : 当前可用的"临时栈槽"偏移（相对于 SP，负数表示向下增长）
  prog             : 要编译的表达式
  is_tail          : 布尔值，表示当前表达式是否处于"尾位置"
                     如果是，则生成的 Call 将优化为尾调用（复用栈帧）
*)
let rec compile_exp ?(curr_stack_size = 0) defns tab stack_index (prog : expr)
    is_tail =
  match prog with
  | Num n -> [ Mov (Reg X0, operand_of_num n) ]
  | True -> [ Mov (Reg X0, operand_of_bool true) ]
  | False -> [ Mov (Reg X0, operand_of_bool false) ]
  | Call (f, args) when not is_tail ->
      let num_args = List.length args in
      (* 八字节每个参数(通用寄存器) *)
      let arg_space = align_to_16 (num_args * 8) in
      (* 临时保存函数指针的槽位 *)
      let save_slot = stack_index in
      (* 编译函数值，检查类型，保存地址 *)
      let compile_f =
        compile_exp ~curr_stack_size defns tab stack_index f false
        @ ensure_type_is_func X0
        @ [
            (* 去除防伪标识 *)
            Sub (Reg X8, Reg X0, Imm func_tagged.tag);
            (* 将函数指针保存到栈，防止被后面覆盖 *)
            Str (X8, BaseOffset (Sp, save_slot));
          ]
      in
      (* 编译参数，保存到栈 *)
      (* 每个参数占用 8 字节，参数区从 stack_index 开始向下增长 *)
      let arg_start_slot = stack_index - 8 in
      let compile_args =
        args
        |> List.mapi (fun i arg ->
            let slot_offset = arg_start_slot - (i * 8) in
            compile_exp ~curr_stack_size defns tab slot_offset arg false
            @ [ Str (X0, BaseOffset (Sp, slot_offset)) ])
        |> List.concat
      in
      (* 把参数从"保存槽"搬运到栈顶参数区（被调函数会从那里读取） *)
      (* 所有参数必须由调用者提前放到栈顶，被调函数从栈里读取 *)
      let move_args =
        args
        |> List.mapi (fun i _ ->
            let slot_offset = arg_start_slot - (i * 8) in
            (* 在 SP 还没有降下来之前，提前把参数写到"未来的新栈顶"位置上 *)
            let target_offset = (i * 8) - arg_space in
            [
              Ldr (X9, BaseOffset (Sp, slot_offset));
              Str (X9, BaseOffset (Sp, target_offset));
            ])
        |> List.concat
      in
      compile_f @ compile_args @ move_args
      (* 恢复函数地址，调整栈，调用，恢复栈 *)
      @ [
          (* 恢复函数地址到 X8 *)
          (* 目标寄存器，稍后 Blr X8 要用它作为跳转地址 *)
          Ldr (X8, BaseOffset (Sp, save_slot));
          (* 向下分配参数区 *)
          (*
            SP 下降了arg_space 字节，原先在 SP 下方的参数区
            现在变成了 SP 正上方（正偏移）的数据

            被调函数进来后，它的 prologue 会再次降低 SP
            而这些参数就正好躺在它栈帧的高地址边界上
          *)
          Sub (Reg Sp, Reg Sp, Imm arg_space);
          (* 间接调用函数 *)
          Blr X8;
          (* 释放参数区 *)
          Add (Reg Sp, Reg Sp, Imm arg_space);
        ]
  (* Tail Call Optimization *)
  (*
    当 Call 出现在尾位置（如 if 的 then/else 分支、函数体末尾、do 最后一句）
    我们不生成普通调用（Blr + Ret），而是复用当前栈帧，直接跳转（Br）
  *)
  | Call (f, args) when is_tail ->
      let num_args = List.length args in
      (* 把函数地址存在参数区后面 *)
      let save_slot = curr_stack_size + (num_args * 8) in
      compile_exp ~curr_stack_size defns tab stack_index f false
      @ ensure_type_is_func X0
      @ [
          Sub (Reg X8, Reg X0, Imm func_tagged.tag);
          Str (X8, BaseOffset (Sp, save_slot));
        ]
      (* 编译参数，直接写入当前函数的参数区，即 curr_stack_size + idx*8 *)
      @ (args |> List.rev
        |> List.mapi (fun i arg ->
            (* 从最后一个参数开始往前，防止参数被覆盖导致计算错误 *)
            let param_idx = num_args - 1 - i in
            let param_offset = curr_stack_size + (param_idx * 8) in
            compile_exp ~curr_stack_size defns tab stack_index arg false
            @ [ Str (X0, BaseOffset (Sp, param_offset)) ])
        |> List.concat)
      @ [
          (* 取出自己的函数地址，恢复栈帧，然后跳转，继续复用栈 *)
          Ldr (X8, BaseOffset (Sp, save_slot));
          Ldp (X29, X30, Sp, curr_stack_size, PostIndex);
          Br X8;
        ]
  | Let (var, e, body) ->
      compile_exp ~curr_stack_size defns tab stack_index e false
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp ~curr_stack_size defns
          (Symtab.add var stack_index tab)
          (stack_index - 8) body is_tail
  | Var var -> (
      match Symtab.find_opt var tab with
      | Some addr -> [ Ldr (X0, BaseOffset (Sp, addr)) ]
      | None ->
          if is_defn defns var then
            [
              Adrp (X0, defn_label var);
              AddLabel (X0, X0, defn_label var);
              Orr (Reg X0, Reg X0, Imm func_tagged.tag);
            ]
          else raise (Compile_error ("Undefined variable: " ^ var)))
  | Prim2 (Cons, e1, e2) ->
      compile_exp ~curr_stack_size defns tab stack_index e1 false
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ compile_exp ~curr_stack_size defns tab (stack_index - 8) e2 false
      @ [
          Ldr (X9, BaseOffset (Sp, stack_index));
          Str (X9, BaseOffset (X19, 0));
          Str (X0, BaseOffset (X19, 8));
          Mov (Reg X0, Reg X19);
          Orr (Reg X0, Reg X0, Imm pair_tagged.tag);
          Add (Reg X19, Reg X19, Imm 16);
        ]
  | Prim1 (Car, e) ->
      compile_exp ~curr_stack_size defns tab stack_index e false
      @ ensure_type_is_pair X0
      @ [ Ldr (X0, BaseOffset (X0, -pair_tagged.tag)) ]
  | Prim1 (Cdr, e) ->
      compile_exp ~curr_stack_size defns tab stack_index e false
      @ ensure_type_is_pair X0
      @ [ Ldr (X0, BaseOffset (X0, -pair_tagged.tag + 8)) ]
  | Prim1 (Display, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      @ [
          Bl (extern_function "print_value"); Mov (Reg X0, operand_of_bool true);
        ]
  | Prim1 (Add1, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      @ ensure_type_is_num X0
      @ [ Add (Reg X0, Reg X0, operand_of_num 1) ]
  | Prim1 (Sub1, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      @ ensure_type_is_num X0
      @ [ Sub (Reg X0, Reg X0, operand_of_num 1) ]
  | Prim1 (Not, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      @ [ Cmp (Reg X0, operand_of_bool false) ]
      @ cond_to_bool "eq"
  | Prim1 (ZeroP, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      (* 类型检查 *)
      @ ensure_type_is_num X0
      @ [ Cmp (Reg X0, operand_of_num 0) ]
      @ cond_to_bool "eq"
  | Prim1 (NumP, arg) ->
      compile_exp ~curr_stack_size defns tab stack_index arg false
      @ [
          (* And : 只有在都是1的情况下才输出1 *)
          And (Reg X0, Reg X0, Imm num_tagged.mask);
          Cmp (Reg X0, Imm num_tagged.tag);
        ]
      @ cond_to_bool "eq"
  | If (test_exp, then_exp, else_exp) ->
      let else_label = gensym "else" in
      let continue_label = gensym "continue" in
      compile_exp ~curr_stack_size defns tab stack_index test_exp false
      @ [ Cmp (Reg X0, operand_of_bool false); Beq else_label ]
      @ compile_exp ~curr_stack_size defns tab stack_index then_exp is_tail
      @ [ B continue_label; Label else_label ]
      @ compile_exp ~curr_stack_size defns tab stack_index else_exp is_tail
      @ [ Label continue_label ]
  | Prim2 (op, e1, e2) when List.mem op [ Plus; Minus; Eq; Gt; Lt ] ->
      let e1_instrs =
        compile_exp ~curr_stack_size defns tab stack_index e1 false
      in
      let e2_instrs =
        compile_exp ~curr_stack_size defns tab (stack_index - 8) e2 false
      in
      let logic =
        match op with
        | Plus ->
            ensure_type_is_num X0
            @ [
                Ldr (X1, BaseOffset (Sp, stack_index));
                Add (Reg X0, Reg X1, Reg X0);
              ]
        | Minus ->
            ensure_type_is_num X0
            @ [
                Ldr (X1, BaseOffset (Sp, stack_index));
                Sub (Reg X0, Reg X1, Reg X0);
              ]
        | Eq ->
            [ Ldr (X1, BaseOffset (Sp, stack_index)); Cmp (Reg X1, Reg X0) ]
            @ cond_to_bool "eq"
        | Gt ->
            [ Ldr (X1, BaseOffset (Sp, stack_index)); Cmp (Reg X1, Reg X0) ]
            @ cond_to_bool "gt"
        | Lt ->
            [ Ldr (X1, BaseOffset (Sp, stack_index)); Cmp (Reg X1, Reg X0) ]
            @ cond_to_bool "lt"
        | _ -> []
      in
      e1_instrs
      @ (if op = Plus || op = Minus then ensure_type_is_num X0 else [])
      @ [ Str (X0, BaseOffset (Sp, stack_index)) ]
      @ e2_instrs @ logic
  | Do exps ->
      let len = List.length exps in
      List.mapi
        (fun i exp ->
          compile_exp ~curr_stack_size defns tab stack_index exp
            (if i = len - 1 then is_tail else false))
        exps
      |> List.concat
  | Prim0 NewLine ->
      [
        Bl (extern_function "print_newline"); Mov (Reg X0, operand_of_bool true);
      ]
  | Prim0 ReadNum -> [ Bl (extern_function "read_num") ]
  | _ -> raise (Compile_error "Unsupported expression")

let compile_defn defns defn =
  let num_args = List.length defn.args in
  (* 确保不超过 504 字节的偏移限制 *)
  let stack_size = align_to_16 (8 * (num_args + 12)) in
  let prologue =
    [
      P2align 3;
      Label (defn_label defn.name);
      Stp (X29, X30, Sp, -stack_size, PreIndex);
    ]
  in
  let ftab =
    defn.args
    |> List.mapi (fun i arg -> (arg, stack_size + (8 * i)))
    |> Symtab.of_list
  in
  prologue
  @ compile_exp ~curr_stack_size:stack_size defns ftab (stack_size - 16)
      defn.body true
  @ [ Ldp (X29, X30, Sp, stack_size, PostIndex); Ret ]

let compile program =
  (* 不要省栈大小 *)
  let entry_stack_size = 496 in
  let prog = program_of_s_exps program in
  [ Text; Global "entry"; P2align 2 ]
  @ List.concat_map extern_function_bridge
      [ "error"; "read_num"; "print_value"; "print_newline" ]
  @ [
      Label "entry";
      (* 入口栈帧 *)
      Stp (X29, X30, Sp, -entry_stack_size, PreIndex);
      (* 保存运行时传入的堆指针 -- Heap *)
      Mov (Reg X19, Reg X0);
    ]
  @ compile_exp ~curr_stack_size:entry_stack_size prog.defns Symtab.empty
      (entry_stack_size - 16) prog.body true
  @ [ Ldp (X29, X30, Sp, entry_stack_size, PostIndex); Ret ]
  @ List.concat_map (compile_defn prog.defns) prog.defns
  |> List.map string_of_directive
  |> String.concat "\n"
