
open Ast

let string_of_varid = string_of_int

let string_of_const t =
  match t with
  | Magic -> "magic"
  | Unit  -> "()"
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Char c -> Printf.sprintf "'%c'" c
  | Atom a -> Cduce.string_of_type a
