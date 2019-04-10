
open Ast

type dir =
    | LApp | RApp | LPair | RPair | PFst | PSnd | Dbg of string

type path = dir list

exception Invalid_path

val follow_path : ('c, 'b, 'a) t -> path -> ('c, 'b, 'a) t

type env = typ ExprMap.t
val empty_env : env
val is_bottom : env -> bool

(*type logs_data = { ignored:int ; visited:int }
val get_logs : exprid -> logs_data
val set_logs : exprid -> logs_data -> unit
val all_logs : unit -> (exprid * logs_data) Seq.t
val clear_logs : unit -> unit*)

exception Ill_typed of string

val back_typeof : env -> annot_expr -> typ -> path -> typ (* Equivalent to Env in the paper *)
val typeof : env -> annot_expr -> typ
val refine_env : env -> annot_expr -> typ -> env
