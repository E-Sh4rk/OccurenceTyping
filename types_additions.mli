
open Ast

type env = typ ExprMap.t

val square : typ -> typ -> typ

val back_typeof : env -> expr -> typ -> path -> typ

val typeof : env -> expr -> typ
