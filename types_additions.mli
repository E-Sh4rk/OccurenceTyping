
open Ast

val conj : typ list -> typ
val disj : typ list -> typ

val square : typ -> typ -> typ (* Does not work with polymorphism yet *)
