
module CD = Cduce_lib

type typ = CD.Types.t
type node = CD.Types.Node.t

val printf : typ -> unit
val dump : Format.formatter -> typ -> unit
val string_of_type : typ -> string
val string_of_node : node -> string
val descr : node -> typ
val cons : typ -> node

val cup : typ -> typ -> typ
val cap : typ -> typ -> typ

val mk_var : bool -> string -> typ
val mk_atom : string -> typ
val mk_list: node -> node

val mk_record : bool -> (string * node) list -> typ
val get_field : typ -> string -> typ
val all_fields : typ -> string list

val mk_arrow : node -> node -> typ
val domain : typ -> typ
val apply : typ -> typ -> typ
val dnf : typ -> (typ * typ) list list

val is_empty : typ -> bool
val non_empty: typ -> bool
val subtype  : typ -> typ -> bool
val disjoint : typ -> typ -> bool
val equiv : typ -> typ -> bool
