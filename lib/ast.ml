open S_exp

(*
  后面的数字代表Arity（元数），用来描述函数/操作符接受参数个数
  比如 ReadNum 和 NewLine 不接受参数，所以是 prim+0
*)
type prim0 = ReadNum | NewLine

let prim0_of_string = function
  | "readnum" -> Some ReadNum
  | "newline" -> Some NewLine
  | _ -> None

type prim1 = Add1 | Sub1 | ZeroP | NumP | Not | Car | Cdr | Display

let prim1_of_string = function
  | "add1" -> Some Add1
  | "sub1" -> Some Sub1
  | "zero?" -> Some ZeroP
  | "number?" -> Some NumP
  | "not" -> Some Not
  | "car" -> Some Car
  | "cdr" -> Some Cdr
  | "display" -> Some Display
  | _ -> None

type prim2 = Plus | Minus | Eq | Lt | Gt | Cons

let prim2_of_string = function
  | "<" -> Some Lt
  | ">" -> Some Gt
  | "cons" -> Some Cons
  | "+" -> Some Plus
  | "-" -> Some Minus
  | "=" -> Some Eq
  | _ -> None

(* 后端真实看到的类型 -- 无 Lambda *)
type expr =
  | Prim0 of prim0
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Do of expr list
  | Num of int
  | Var of string
  | Call of expr * expr list
  | True
  | False
  | Closure of string

(* Lambda 构造器 -- 只有前端解析时存在的类型 *)
type expr_lam =
  | Prim0 of prim0
  | Prim1 of prim1 * expr_lam
  | Prim2 of prim2 * expr_lam * expr_lam
  | Let of string * expr_lam * expr_lam
  | If of expr_lam * expr_lam * expr_lam
  | Do of expr_lam list
  | Num of int
  | Var of string
  | Call of expr_lam * expr_lam list
  | True
  | False
  | Lambda of string list * expr_lam

exception BadSExpression of s_exp
exception BadExpression of expr

(* function *)
(* (define add (a b) (+ a b)) => defns = [{name = "add"; args = ["a"; "b"]; body = Lst [Sym "+"; Sym "a"; Sym "b"]}] *)
type defn = { name : string; args : string list; body : expr }
type program = { defns : defn list; body : expr }

(* 遍历列表并查询是否符合条件 *)
let is_defn defns name = List.exists (fun d -> d.name = name) defns

(* 查找符合条件的函数定义 *)
let get_defn defns name = List.find (fun d -> d.name = name) defns

(* 是 S_exp.Sym 类型 *)
let is_sym e = match e with Sym _ -> true | _ -> false

(* 转换到string *)
let as_sym e = match e with Sym s -> s | _ -> raise Not_found

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

(* 第一次进行parse，返回 expr_lam *)
let rec expr_lam_of_s_exp : s_exp -> expr_lam = function
  | Num x -> Num x
  | Sym "#t" -> True
  | Sym "#f" -> False
  | Sym var -> Var var
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; exp ] ]; body ] ->
      Let (var, expr_lam_of_s_exp exp, expr_lam_of_s_exp body)
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      Do (List.map expr_lam_of_s_exp exps)
  | Lst [ Sym "if"; test_s; then_s; else_s ] ->
      If
        ( expr_lam_of_s_exp test_s,
          expr_lam_of_s_exp then_s,
          expr_lam_of_s_exp else_s )
  (* 如果是 (prim0) 用 Option.is_some 匹配是谁 *)
  | Lst [ Sym prim ] when Option.is_some (prim0_of_string prim) ->
      (* Option.get 从 Some x 取出 x *)
      Prim0 (Option.get (prim0_of_string prim))
  | Lst [ Sym prim; arg ] when Option.is_some (prim1_of_string prim) ->
      Prim1 (Option.get (prim1_of_string prim), expr_lam_of_s_exp arg)
  | Lst [ Sym prim; arg1; arg2 ] when Option.is_some (prim2_of_string prim) ->
      Prim2
        ( Option.get (prim2_of_string prim),
          expr_lam_of_s_exp arg1,
          expr_lam_of_s_exp arg2 )
      (* 先匹配有没有lambda *)
      (* for_all 检查列表里每个都满足条件 *)
  | Lst [ Sym "lambda"; Lst args; body ] when List.for_all is_sym args ->
      (* 涉及到的变量转换成字符串 *)
      Lambda (List.map as_sym args, expr_lam_of_s_exp body)
  | Lst (f :: args) ->
      Call (expr_lam_of_s_exp f, List.map expr_lam_of_s_exp args)
  | e -> raise (BadSExpression e)

(*第二次进行parse，返回 expr，编译器可使用 *)
let rec expr_of_expr_lam (defns : defn list ref) : expr_lam -> expr = function
  | Num x -> Num x
  | Var s -> Var s
  | True -> True
  | False -> False
  | If (test_exp, then_exp, else_exp) ->
      If
        ( expr_of_expr_lam defns test_exp,
          expr_of_expr_lam defns then_exp,
          expr_of_expr_lam defns else_exp )
  | Let (var, exp, body) ->
      Let (var, expr_of_expr_lam defns exp, expr_of_expr_lam defns body)
  | Prim0 p -> Prim0 p
  | Prim1 (p, e) -> Prim1 (p, expr_of_expr_lam defns e)
  | Prim2 (p, e1, e2) ->
      Prim2 (p, expr_of_expr_lam defns e1, expr_of_expr_lam defns e2)
  | Do exps -> Do (List.map (expr_of_expr_lam defns) exps)
  | Call (exp, args) ->
      Call (expr_of_expr_lam defns exp, List.map (expr_of_expr_lam defns) args)
  | Lambda (args, body) ->
      (* 第 x 个闭包 *)
      let name = gensym "_lambda" in
      (* 处理body *)
      let body = expr_of_expr_lam defns body in
      (* 把整个处理完的({name="_lambda__0"; args=["x"]; body=...})放到defns内 *)
      (* 往全局的函数定义表 (program_of_s_exps 里面的那个) 里追加一条记录 *)
      defns := { name; args; body } :: !defns;
      (* 把自己当作闭包返回 *)
      (* 编译器会识别这个闭包，如果没有用args则当作普通变量返回，用了就是闭包 *)
      Closure name

(* 解析表达式列表，返回函数定义列表和剩余表达式 *)
(* 限制：
    - 不能先进行逻辑再 define，也不能在 define 之间穿插逻辑表达
    - 强制要求最后一个表达式必须为程序的主体
    - 不支持嵌套定义 *)
let program_of_s_exps exps =
  (* 全局函数定义 -- 包括 lambda *)
  let defns = ref [] in
  (* 提取参数名 *)
  let rec get_args args =
    (* Lst (Sym name :: args) : 匹配非空列表，第一个内容为name *)
    (* (define (add a b) (+ a b)) => Lst [Sym "define"; Lst [Sym "add"; Sym "x"; Sym "y"]; body] *)
    match args with
    | Sym v :: args -> v :: get_args args
    | e :: _ -> raise (BadSExpression e)
    | [] -> []
  in
  (* 识别函数 *)
  let get_defn = function
    | Lst [ Sym "define"; Lst (Sym name :: args); body ] ->
        let args = get_args args in
        {
          name;
          args;
          (* 这里会把 lambda 也塞进去 *)
          body = body |> expr_lam_of_s_exp |> expr_of_expr_lam defns;
        }
    | e -> raise (BadSExpression e)
  in
  let rec go exps =
    match exps with
    (* 只剩最后一个表达式  *)
    | [ e ] ->
        (* 如果有 lambda 也会被识别 *)
        let body = e |> expr_lam_of_s_exp |> expr_of_expr_lam defns in
        { defns = List.rev !defns; body }
    | d :: exps ->
        let defn = get_defn d in
        (* 找到的函数 *)
        defns := defn :: !defns;
        go exps
    | _ -> raise (BadSExpression (Sym "empty"))
  in
  go exps
