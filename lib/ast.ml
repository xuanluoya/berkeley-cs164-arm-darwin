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
  (* | "plus" -> Some Plus
  | "minus" -> Some Minus
  | "eq" -> Some Eq *)
  | "<" -> Some Lt
  | ">" -> Some Gt
  | "cons" -> Some Cons
  | "+" -> Some Plus
  | "-" -> Some Minus
  | "=" -> Some Eq
  | _ -> None

type expr =
  | Prim0 of prim0
  | Prim1 of prim1 * expr
  | Prim2 of prim2 * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Do of expr list
  | Num of int
  | Var of string
  | Call of string * expr list
  | True
  | False

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

let rec expr_of_s_exp : s_exp -> expr = function
  | Num x -> Num x
  | Sym "#t" -> True
  | Sym "#f" -> False
  | Sym var -> Var var
  | Lst [ Sym "let"; Lst [ Lst [ Sym var; exp ] ]; body ] ->
      Let (var, expr_of_s_exp exp, expr_of_s_exp body)
  | Lst (Sym "do" :: exps) when List.length exps > 0 ->
      Do (List.map expr_of_s_exp exps)
  | Lst [ Sym "if"; test_s; then_s; else_s ] ->
      If (expr_of_s_exp test_s, expr_of_s_exp then_s, expr_of_s_exp else_s)
  (* 如果是 (prim0) 用 Option.is_some 匹配是谁 *)
  | Lst [ Sym prim ] when Option.is_some (prim0_of_string prim) ->
      (* Option.get 从 Some x 取出 x *)
      Prim0 (Option.get (prim0_of_string prim))
  | Lst [ Sym prim; arg ] when Option.is_some (prim1_of_string prim) ->
      Prim1 (Option.get (prim1_of_string prim), expr_of_s_exp arg)
  | Lst [ Sym prim; arg1; arg2 ] when Option.is_some (prim2_of_string prim) ->
      Prim2
        ( Option.get (prim2_of_string prim),
          expr_of_s_exp arg1,
          expr_of_s_exp arg2 )
  | Lst (Sym f :: args) -> Call (f, List.map expr_of_s_exp args)
  | e -> raise (BadSExpression e)

(* 解析表达式列表，返回函数定义列表和剩余表达式 *)
(* 限制：
    - 不能先进行逻辑再 define，也不能在 define 之间穿插逻辑表达
    - 强制要求最后一个表达式必须为程序的主体
    - 不支持嵌套定义 *)
let program_of_s_exps exps =
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
        { name; args; body = expr_of_s_exp body }
    | e -> raise (BadSExpression e)
  in
  let rec go exps defns =
    match exps with
    (* 只剩最后一个表达式  *)
    | [ e ] -> { defns = List.rev defns; body = expr_of_s_exp e }
    | d :: exps -> go exps (get_defn d :: defns)
    | _ -> raise (BadSExpression (Sym "empty"))
  in
  go exps []
