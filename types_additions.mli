
open Ast

type env = typ ExprMap.t
val empty_env : env

val conj : typ list -> typ
val disj : typ list -> typ

val square : typ -> typ -> typ

exception Ill_typed

val back_typeof : env -> expr -> typ -> path -> typ (* Equivalent to t in the paper *)
val typeof : env -> expr -> typ
val refine_env : env -> expr -> typ -> env
