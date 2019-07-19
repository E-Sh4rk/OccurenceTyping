open Ast

val substitute_var : 'a -> ('c, 'b, 'a) t -> ('c, 'b, 'a) t -> ('c, 'b, 'a) t

val abstract_unabstracted_ite : annot_expr -> typ VarIdMap.t -> annot_expr
