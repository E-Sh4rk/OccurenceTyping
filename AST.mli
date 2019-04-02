
type typ = Cduce.typ

type const =
    | Magic
    | Bool of bool
    | Int of int
    | Char of char

type projection = Fst | Snd

type varname = string
type varid = int (* It is NOT De Bruijn indexes, but unique IDs *)

type 'var expr' =
    | Const of const
    | Var of 'var
    | Lambda of typ * 'var * 'var expr'
    | Ite of 'var expr' * typ * 'var expr' * 'var expr'
    | App of 'var expr' * 'var expr'
    | Let of 'var * 'var expr' * 'var expr'
    | Pair of 'var expr' * 'var expr'
    | Projection of projection * 'var expr'

type parser_expr = varname expr'
type expr = varid expr'

val make_lambda_abstraction : 'a list -> typ -> 'a expr' -> 'a expr'

module Expr : sig
    type t = expr
    val compare : t -> t -> int
    val equiv : t -> t -> bool
end
module ExprMap : Map.S with type key = expr

val unique_varid : unit -> varid

val parser_expr_to_expr : parser_expr -> expr

val substitute_var : 'a -> 'a expr' -> 'a expr' -> 'a expr'
