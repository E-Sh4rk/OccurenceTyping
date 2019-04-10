
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
type exprid = int

type annotation = exprid * Lexing.position

(* Could be a better definition but 'cyclic type' ... (actually not) *)
(*
type ('typ, 'v, 't) t =
| Const of 'typ const
| Var of 'v
| Lambda of 'typ * 'v * 't
| RecLambda of 'v * 'typ * 'v * 't
| Ite of 't * 'typ * 't * 't
| App of 't * 't
| Let of 'v * 't * 't
| Pair of 't * 't
| Projection of projection * 't
| Debug of string * 't

type annoted_expr = annotation * (typ, varid, annoted_expr) t
type expr = (typ, varid, expr) t
type parser_expr = annotation * (type_expr, varname, parser_expr) t
*)

type ('a, 'typ, 'v) ast =
| Const of 'typ const
| Var of 'v
| Lambda of 'typ * 'v * ('a, 'typ, 'v) t
| RecLambda of 'v * 'typ * 'v * ('a, 'typ, 'v) t
| Ite of ('a, 'typ, 'v) t * 'typ * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| App of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Let of 'v * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Pair of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Projection of projection * ('a, 'typ, 'v) t
| Debug of string * ('a, 'typ, 'v) t

and ('a, 'typ, 'v) t = 'a * ('a, 'typ, 'v) ast

type annot_expr = (annotation, typ, varid) t
type expr = (unit, typ, varid) t
type parser_expr = (annotation, type_expr, varname) t

module Expr : sig
    type t = expr
    val compare : t -> t -> int
    val equiv : t -> t -> bool
end
module ExprMap : Map.S with type key = expr

type id_map = int StrMap.t

val empty_id_map : id_map

val unique_exprid : unit -> exprid

val unique_varid : unit -> varid

val identifier_of_expr : (annotation, 'a, 'b) t -> exprid

val new_dummy_annot : unit -> annotation

val parser_const_to_const : type_env -> type_expr const -> typ const

val parser_expr_to_annot_expr : type_env -> id_map -> parser_expr -> annot_expr

val unannot : annot_expr -> expr

val substitute_var : 'a -> ('c, 'b, 'a) t -> ('c, 'b, 'a) t -> ('c, 'b, 'a) t

val const_to_typ : typ const -> typ

type parser_element =
| Definition of (string * parser_expr)
| Atoms of string list
| Types of (string * type_expr) list

type parser_program = parser_element list
