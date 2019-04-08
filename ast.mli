
open Types_additions
type typ = Cduce.typ

type 't const =
| Magic
| Unit
| Bool of bool
| Int of int
| Char of char
| Atom of 't

type projection = Fst | Snd

type varname = string
type varid = int (* It is NOT De Bruijn indexes, but unique IDs *)

type ('t, 'v) expr' =
| Const of 't const
| Var of 'v
| Lambda of 't * 'v * ('t, 'v) expr'
| RecLambda of 'v * 't * 'v * ('t, 'v) expr'
| Ite of ('t, 'v) expr' * 't * ('t, 'v) expr' * ('t, 'v) expr'
| App of ('t, 'v) expr' * ('t, 'v) expr'
| Let of 'v * ('t, 'v) expr' * ('t, 'v) expr'
| Pair of ('t, 'v) expr' * ('t, 'v) expr'
| Projection of projection * ('t, 'v) expr'
| Debug of string * ('t, 'v) expr'

type parser_expr = (type_expr, varname) expr'
type expr = (typ, varid) expr'

module Expr : sig
    type t = expr
    val compare : t -> t -> int
    val equiv : t -> t -> bool
end
module ExprMap : Map.S with type key = expr

type id_map = int StrMap.t

val empty_id_map : id_map

val unique_varid : unit -> varid

val parser_const_to_const : type_env -> type_expr const -> typ const

val parser_expr_to_expr : type_env -> id_map -> parser_expr -> expr

val substitute_var : 'a -> ('b, 'a) expr' -> ('b, 'a) expr' -> ('b, 'a) expr'

val const_to_typ : typ const -> typ

type parser_element =
| Definition of (string * parser_expr)
| Atoms of string list
| Types of (string * type_expr) list

type parser_program = parser_element list
