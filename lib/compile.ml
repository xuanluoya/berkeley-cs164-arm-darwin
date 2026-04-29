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

(* 寻找自由变量 *)
let rec fv defns bound (exp : expr) =
  match exp with
  | Var v when not (List.mem v bound) -> [ v ]
  | Let (v, e, body) -> fv defns bound e @ fv defns (v :: bound) body
  | If (te, the, ee) ->
      fv defns bound te @ fv defns bound the @ fv defns bound ee
  | Do es -> List.concat_map (fv defns bound) es
  | Call (f, args) -> fv defns bound f @ List.concat_map (fv defns bound) args
  | Prim1 (_, e) -> fv defns bound e
  | Prim2 (_, e1, e2) -> fv defns bound e1 @ fv defns bound e2
  | Closure f ->
      let defn = get_defn defns f in
      (* Find the free variables in the closure/lambda's body *)
      fv defns (bound @ List.map (fun d -> d.name) defns @ defn.args) defn.body
  | _ -> []

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
      (* +1 -- closure_slot *)
      let arg_space = align_to_16 ((num_args + 1) * 8) in
      (* 被调函数看到的闭包槽偏移 *)
      let closure_slot = num_args * 8 in

      (* 临时栈槽布局 *)
      let code_slot = stack_index in
      let closure_tmp_slot = stack_index - 8 in
      let arg_start_slot = stack_index - 16 in

      let compile_f =
        compile_exp ~curr_stack_size defns tab stack_index f false
        @ ensure_type_is_func X0
        @ [
            (* 暂存闭包指针 *)
            Mov (Reg X10, Reg X0);
            (* 去标签 *)
            Sub (Reg X8, Reg X0, Imm func_tagged.tag);
            (* 取代码入口 *)
            Ldr (X9, BaseOffset (X8, 0));
            (* 保存代码入口 *)
            Str (X9, BaseOffset (Sp, code_slot));
            (* 保存闭包指针到临时槽 *)
            Str (X10, BaseOffset (Sp, closure_tmp_slot));
          ]
      in

      let compile_args =
        args
        |> List.mapi (fun i arg ->
            let slot = arg_start_slot - (i * 8) in
            compile_exp ~curr_stack_size defns tab slot arg false
            @ [ Str (X0, BaseOffset (Sp, slot)) ])
        |> List.concat
      in

      let move_args =
        args
        |> List.mapi (fun i _ ->
            let slot = arg_start_slot - (i * 8) in
            let target = (i * 8) - arg_space in
            [
              Ldr (X9, BaseOffset (Sp, slot)); Str (X9, BaseOffset (Sp, target));
            ])
        |> List.concat
      in

      compile_f @ compile_args @ move_args
      @ [
          Ldr (X8, BaseOffset (Sp, closure_tmp_slot));
          (* 从临时槽取闭包指针 *)
          Str (X8, BaseOffset (Sp, closure_slot - arg_space));
          (* 写入目标闭包槽 *)
          Ldr (X8, BaseOffset (Sp, code_slot));
          (* 恢复代码入口 *)
          Sub (Reg Sp, Reg Sp, Imm arg_space);
          Blr X8;
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
      let code_slot = stack_index in
      let closure_tmp_slot = stack_index - 8 in
      let compile_f =
        compile_exp ~curr_stack_size defns tab stack_index f false
        @ ensure_type_is_func X0
        @ [
            (* 暂存闭包指针 *)
            Mov (Reg X10, Reg X0);
            Sub (Reg X8, Reg X0, Imm func_tagged.tag);
            (* 代码入口 *)
            Ldr (X9, BaseOffset (X8, 0));
            (* 保存代码入口 *)
            Str (X9, BaseOffset (Sp, code_slot));
            (* 保存闭包指针 *)
            Str (X10, BaseOffset (Sp, closure_tmp_slot));
          ]
      in
      let compile_args =
        args
        |> List.mapi (fun i arg ->
            let param_offset = curr_stack_size + (i * 8) in
            compile_exp ~curr_stack_size defns tab stack_index arg false
            @ [ Str (X0, BaseOffset (Sp, param_offset)) ])
        |> List.concat
      in
      (* 写入闭包指针到参数区之后 *)
      let store_closure =
        [
          Ldr (X8, BaseOffset (Sp, closure_tmp_slot));
          Str (X8, BaseOffset (Sp, curr_stack_size + (num_args * 8)));
        ]
      in
      compile_f @ compile_args @ store_closure
      @ [
          (* 恢复代码入口 *)
          Ldr (X8, BaseOffset (Sp, code_slot));
          (* 恢复帧并释放栈帧 *)
          Ldp (X29, X30, Sp, curr_stack_size, PostIndex);
          (* 跳转 *)
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
              (* 在堆上分配一个最小闭包 *)
              Adrp (X0, defn_label var);
              AddLabel (X0, X0, defn_label var);
              (* 内存单元和内存地址不相同 *)
              Str (X0, BaseOffset (X19, 0));
              Mov (Reg X0, Reg X19);
              Orr (Reg X0, Reg X0, Imm func_tagged.tag);
              Add (Reg X19, Reg X19, Imm 8);
            ]
          else raise (Compile_error ("Undefined variable: " ^ var)))
  (* 发现闭包，存入heap *)
  | Closure f ->
      let defn = get_defn defns f in
      let fvs =
        fv defns (List.map (fun d -> d.name) defns @ defn.args) defn.body
      in
      let missing = List.filter (fun v -> not (Symtab.mem v tab)) fvs in
      if missing <> [] then
        raise
          (Compile_error
             ("Free variables not in scope: " ^ String.concat ", " missing))
      else
        let fv_store =
          fvs
          |> List.mapi (fun i var ->
              let offset = 8 * (i + 1) in
              [
                Ldr (X0, BaseOffset (Sp, Symtab.find var tab));
                Str (X0, BaseOffset (X19, offset));
              ])
          |> List.flatten
        in
        (* Let function tag write into heap stack 0 *)
        [
          Adrp (X0, defn_label f);
          AddLabel (X0, X0, defn_label f);
          (* 闭包对象的第一个字段是“代码指针”，这块内存我们就用 [X19 + 0] 表示 *)
          Str (X0, BaseOffset (X19, 0));
        ]
        @ fv_store
        (* 把堆指针打上函数标签，返回给调用者 *)
        @ [
            (* X19 是目前全局堆指针，它指向可用的空闲堆内存的起始地址 *)
            Mov (Reg X0, Reg X19);
            (* 写入 function tag *)
            Orr (Reg X0, Reg X0, Imm func_tagged.tag);
            (* 移动堆指针，为下次分配预留空间 -- +1 是因为还有一个函数指针 *)
            Add (Reg X19, Reg X19, Imm (8 * (List.length fvs + 1)));
          ]
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
  (* 自由变量们 *)
  let fvs = fv defns (List.map (fun d -> d.name) defns @ defn.args) defn.body in
  let num_args = List.length defn.args in
  let num_fvs = List.length fvs in

  (* 确保不超过 504 字节的偏移限制 -- 多出来的12字节是缓冲区 *)
  let stack_size = align_to_16 (8 * (num_args + num_fvs + 12)) in

  (* 自由变量与args的符号表 *)
  let ftab =
    defn.args @ fvs
    |> List.mapi (fun i var -> (var, stack_size + (8 * i)))
    |> Symtab.of_list
  in

  (* 闭包指针紧接在所有显式参数之后 *)
  let closure_ptr_slot = stack_size + (8 * num_args) in

  (* 将自由变量从闭包复制到栈上 *)
  let fvs_to_stack =
    [
      Ldr (X9, BaseOffset (Sp, closure_ptr_slot));
      Sub (Reg X9, Reg X9, Imm func_tagged.tag);
      (* 跳过函数指针的位置 -- 指向第一个自由变量 *)
      Add (Reg X9, Reg X9, Imm 8);
    ]
    @ List.concat
        (List.mapi
           (fun i _ ->
             [
               Ldr (X8, BaseOffset (X9, 8 * i));
               Str (X8, BaseOffset (Sp, stack_size + (8 * (num_args + i))));
             ])
           fvs)
  in
  let prologue =
    [
      P2align 3;
      Label (defn_label defn.name);
      Stp (X29, X30, Sp, -stack_size, PreIndex);
    ]
  in
  prologue @ fvs_to_stack
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
