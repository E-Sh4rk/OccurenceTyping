
open Ast

type dir =
    | LApp | RApp | LPair | RPair | PFst | PSnd | Dbg of string

type path = dir list

exception Invalid_path

val follow_path : expr -> path -> expr

type env = typ ExprMap.t
val empty_env : env
val is_bottom : env -> bool

(*type logs_data = { ignored:int ; visited:int }
val get_logs : expr -> logs_data
val set_logs : expr -> logs_data -> unit
val all_logs : unit -> (expr * logs_data) Seq.t
val clear_logs : unit -> unit*)

exception Ill_typed of string

val back_typeof : env -> expr -> typ -> path -> typ (* Equivalent to Env in the paper *)
val typeof : env -> expr -> typ
val refine_env : env -> expr -> typ -> env
