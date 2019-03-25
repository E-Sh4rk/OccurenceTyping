
module CD = Cduce_lib

type typ = CD.Types.t
type node = CD.Types.Node.t

val printf : typ -> unit
val descr : node -> typ
val cons : typ -> node

val cup : typ -> typ -> typ
val cap : typ -> typ -> typ

val mk_var : bool -> string -> typ
val mk_atom : string -> typ
val mk_list: node -> node
