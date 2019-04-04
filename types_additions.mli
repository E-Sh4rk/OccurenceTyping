
open Cduce

(* Construction of types *)

type type_base =
    TInt | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty

type type_expr =
| TBase of type_base
| TCustom of string
| TPair of type_expr * type_expr
| TArrow of type_expr * type_expr
| TCup of type_expr * type_expr
| TCap of type_expr * type_expr
| TDiff of type_expr * type_expr
| TNeg of type_expr

val type_base_to_typ : type_base -> typ

val type_expr_to_typ : type_expr -> typ

(* Operations on types *)

val conj : typ list -> typ
val disj : typ list -> typ

val square : typ -> typ -> typ (* Does not work with polymorphism yet *)
