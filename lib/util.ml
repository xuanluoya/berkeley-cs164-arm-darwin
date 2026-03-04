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
