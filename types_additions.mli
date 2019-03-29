
open Ast

type env = typ ExprMap.t

val square : typ -> typ -> typ

val back_typeof : env -> expr -> typ -> path -> typ (* Equivalent to t in the paper *)

val typeof : env -> expr -> typ

val refine_env : env -> expr -> typ -> env
