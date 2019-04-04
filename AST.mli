
open Types_additions
type typ = Cduce.typ

type const =
    | Magic
    | Unit
    | Bool of bool
    | Int of int
    | Char of char

type projection = Fst | Snd

type varname = string
type varid = int (* It is NOT De Bruijn indexes, but unique IDs *)

type ('t, 'v) expr' =
| Const of const
| Var of 'v
| Lambda of 't * 'v * ('t, 'v) expr'
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

val unique_varid : unit -> varid

val parser_expr_to_expr : parser_expr -> expr

val substitute_var : 'a -> ('b, 'a) expr' -> ('b, 'a) expr' -> ('b, 'a) expr'

type parser_element =
| Definition of (string * parser_expr)
| Atoms of string list
| Types of (string * type_expr) list

type parser_program = parser_element list
