
open Cduce
open Ast
open Types_additions

type dir =
    | LApp | RApp | LPair | RPair | PFst | PSnd | Dbg of string
    | RProj of string | LRecord of string | RRecord of string

type path = dir list

exception Invalid_path

let rec follow_path (a,e) p =
    match e, p with
    | e, [] -> (a,e)
    | App (e,_), LApp::p
    | App (_,e), RApp::p
    | Pair (e,_), LPair::p
    | Pair (_,e), RPair::p
    | Projection (Fst, e), PFst::p
    | Projection (Snd, e), PSnd::p
    | Debug (_, e), (Dbg _)::p
    | Projection (Field _, e), (RProj _)::p
    | RecordUpdate (e,_,_), (LRecord _)::p
    | RecordUpdate (_,_,Some e), (RRecord _)::p
    -> follow_path e p
    | _, LApp::_ | _, RApp::_
    | _, LPair::_ | _, RPair::_
    | _, PFst::_ | _, PSnd::_
    | _, (Dbg _)::_ | _, (RProj _)::_
    | _, (LRecord _)::_ | _, (RRecord _)::_ -> raise Invalid_path

type env = typ ExprMap.t
let empty_env = ExprMap.empty
let is_bottom env =
    let is_bottom (_,v) = is_empty v in
    List.exists is_bottom (ExprMap.bindings env)

let add_atoms_to_env env atoms tenv =
    let add_atom acc atom =
        ExprMap.add ((), Const (Atom atom)) (get_atom tenv atom) acc
    in
    List.fold_left add_atom env atoms

type logs_data = { ignored:int ; visited:int ; position:Position.t }
let logs = Hashtbl.create 25
let default_logs_data pos = { ignored = 0 ; visited = 0 ; position = pos }
let clear_logs () =
    (* Reset restore the initial bucket table size,
    so we can iterate more efficiently on the table *)
    Hashtbl.reset logs
let all_logs () = Hashtbl.to_seq logs
let get_logs id = Hashtbl.find_opt logs id
let get_logs_expr e =
    match Hashtbl.find_opt logs (identifier_of_expr e) with
    | None -> default_logs_data (position_of_expr e)
    | Some ld -> ld
let set_logs e ld =
    Hashtbl.replace logs (identifier_of_expr e) ld

exception Ill_typed of Position.t * string

let rec all_paths_for_expr rev_prefix e =
    match snd e with
    | App (e1, e2) ->
        let p1 = all_paths_for_expr (LApp::rev_prefix) e1 in
        let p2 = all_paths_for_expr (RApp::rev_prefix) e2 in
        (List.rev rev_prefix)::(p1@p2)
    | Let _ ->
        raise (Ill_typed (position_of_expr e, "There can't be apparent lets in tests!"))
    | Pair (e1, e2) ->
        let p1 = all_paths_for_expr (LPair::rev_prefix) e1 in
        let p2 = all_paths_for_expr (RPair::rev_prefix) e2 in
        (List.rev rev_prefix)::(p1@p2)
    | Projection (Fst, e) ->
        (List.rev rev_prefix)::(all_paths_for_expr (PFst::rev_prefix) e)
    | Projection (Snd, e) ->
        (List.rev rev_prefix)::(all_paths_for_expr (PSnd::rev_prefix) e)
    | Projection (Field str, e) ->
        (List.rev rev_prefix)::(all_paths_for_expr ((RProj str)::rev_prefix) e)
    | RecordUpdate (e1, str, None) ->
        (List.rev rev_prefix)::(all_paths_for_expr ((LRecord str)::rev_prefix) e1)
    | RecordUpdate (e1, str, Some e2) ->
        let p1 = all_paths_for_expr ((LRecord str)::rev_prefix) e1 in
        let p2 = all_paths_for_expr ((RRecord str)::rev_prefix) e2 in
        (List.rev rev_prefix)::(p1@p2)
    | Debug (str, e) ->
        (List.rev rev_prefix)::(all_paths_for_expr ((Dbg str)::rev_prefix) e)
    | Const _ | Var _ | Lambda _ | RecLambda _ | Ite _ -> [List.rev rev_prefix]

module PathMap = Map.Make(struct type t = path let compare = compare end)

let rec back_typeof_rev_open self (memo_t, env, e, t, p) =
    let typeof = typeof_memo memo_t in
    let typeof p = typeof env (follow_path e (List.rev p)) in
    let self = fun p -> self (memo_t, env, e, t, p) in
    let t = match p with
    | [] -> t
    | LApp::p ->
        let left = typeof (RApp::p) in
        let right = self p in
        let t = mk_arrow (cons left) (cons right) in
        let not_t = mk_arrow (cons left) (cons (neg right)) in
        diff t not_t
    | RApp::p -> square (typeof (LApp::p)) (self p)
    | LPair::p -> pi1 (self p)
    | RPair::p -> pi2 (self p)
    | PFst::p -> mk_times (cons (self p)) any_node
    | PSnd::p -> mk_times any_node (cons (self p))
    | (Dbg str)::p -> let res = self p in Format.printf "%s (back_typeof): " str ; Utils.print_type res; res
    | (RProj str)::p -> mk_record true [str, cons (self p)]
    | (RRecord str)::p -> get_field (self p) str
    | (LRecord str)::p ->
        let left = remove_field (self p) str in
        let right = mk_record false [str, any_or_absent_node] in
        merge_records left right
    in
    cap t (typeof p)

and back_typeof_no_memo _ =
    let back_typeof_rev = Utils.do_not_memoize back_typeof_rev_open in
    fun env e t p -> back_typeof_rev (Hashtbl.create 1, env, e, t, List.rev p)

and optimized_back_typeof env e t ps =
    let n = List.length ps in
    let path_select (_,_,_,_,p) = p in
    let back_typeof_rev = Utils.memoize back_typeof_rev_open path_select (Hashtbl.create n) in
    let memo_to = Hashtbl.create n in
    List.map (fun p -> back_typeof_rev (memo_to, env, e, t, List.rev p)) ps

and typeof_open self (env, e) =
    (* The rule that states that 'every expression has type bottom in the bottom environment'
       is integrated in the Ite case for efficiency reasons. *)

    let pos = position_of_expr e in

    let type_lambda (s,t,v,e) =
        if subtype t arrow_any then
            let env = match s with
            | None -> env
            | Some s -> ExprMap.add ((), Var s) t env
            in
            let dnf = dnf t in
            let valid_type conj =
                let is_valid (s,t) =
                    let env = ExprMap.add ((), Var v) s env in
                    subtype (self (env, e)) t
                in
                List.for_all is_valid conj
            in
            if List.exists valid_type dnf then t
            else raise (Ill_typed (pos, "Wrong type for the lambda-abstraction."))
        else raise (Ill_typed (pos, "A lambda-abstraction must have an arrow type!"))
    in

    match ExprMap.find_opt (unannot e) env with
    | Some t -> t
    | None ->
        begin match snd e with
        | Const c -> const_to_typ c
        | Lambda (t,v,e) -> type_lambda (None,t,v,e)
        | RecLambda (s,t,v,e) -> type_lambda (Some s,t,v,e)
        | App (e1, e2) ->
            let t1 = self (env, e1) in
            let t2 = self (env, e2) in
            if subtype t2 (domain t1) then apply t1 t2
            else raise (Ill_typed (pos, "Bad domain for the application."))
        | Ite (e,t,e1,e2) ->
            (* No need to check the type of e here: it is already checked in refine_env *)
            let env1 = refine_env env e t in
            let env2 = refine_env env e (neg t) in
            (* We do some marking in order to detect unreachable code *)
            let logs1 = get_logs_expr e1 in
            let logs2 = get_logs_expr e2 in
            let t1 = if is_bottom env1
            then (set_logs e1 {logs1 with ignored=logs1.ignored+1} ; empty)
            else (set_logs e1 {logs1 with visited=logs1.visited+1} ; self (env1, e1)) in
            let t2 = if is_bottom env2
            then (set_logs e2 {logs2 with ignored=logs2.ignored+1} ; empty)
            else (set_logs e2 {logs2 with visited=logs2.visited+1} ; self (env2, e2)) in
            cup t1 t2
        | Let (v, e1, e2) ->
            let env = ExprMap.add (unannot e1) (self (env, e1)) env in
            self (env, substitute_var v e1 e2)
        | Var _ -> assert false
        | Pair (e1, e2) ->
            let t1 = self (env, e1) in
            let t2 = self (env, e2) in
            mk_times (cons t1) (cons t2)
        | Projection (Fst, e) ->
            let t = self (env, e) in
            if subtype t pair_any then pi1 t
            else raise (Ill_typed (pos, "Fst can only be applied to a pair."))
        | Projection (Snd, e) ->
            let t = self (env, e) in
            if subtype t pair_any then pi2 t
            else raise (Ill_typed (pos, "Snd can only be applied to a pair."))
        | Projection (Field str, e) ->
            let t = self (env, e) in
            if subtype t record_any
            then
                try get_field t str
                with Not_found ->
                raise (Ill_typed (pos, Printf.sprintf "The record does not surely contains the field %s." str))
            else raise (Ill_typed (pos, "Field projection can only be applied to a record."))
        | RecordUpdate (e1, str, e2) ->
            let t = self (env, e1) in
            if subtype t record_any
            then begin
                match e2 with
                | None -> remove_field t str
                | Some e2 ->
                    let t' = mk_record false [str, cons (self (env, e2))] in
                    merge_records t t'
            end else raise (Ill_typed (pos, "Field assignation can only be applied to a record."))
        | Debug (str, e) ->
            let res = self (env, e) in
            Format.printf "%s (typeof): " str ; Utils.print_type res ;
            res
        end

and typeof_no_memo _ =
    let typeof = Utils.do_not_memoize typeof_open in
    fun env e -> typeof (env, e)

and typeof_memo ht =
    let path_select (_,e) = identifier_of_expr e in
    let typeof = Utils.memoize typeof_open path_select ht in
    fun env e -> typeof (env, e)

and refine_env env e t =
    (* No need to continue memoisation here because back_typeof never goes into Ite *)
    let paths = all_paths_for_expr [] e in
    (* Deduce a type for each path *)
    let paths_t = optimized_back_typeof env e t paths in
    let paths_t = List.fold_left2 (fun acc p t -> PathMap.add p t acc) PathMap.empty paths paths_t in 
    (* Build a map from sub-expressions (occurrences) to paths *)
    let add_path acc p =
        let e = unannot (follow_path e p) in
        let v = match ExprMap.find_opt e acc with
        | None -> [p]
        | Some ps -> p::ps
        in ExprMap.add e v acc
    in
    let map = List.fold_left add_path ExprMap.empty paths in
    (* Refine the type for each expression *)
    let refine_for_expr acc (e, paths) =
        let types = List.map (fun p -> PathMap.find p paths_t) paths in
        ExprMap.add e (conj types) acc
    in
    List.fold_left refine_for_expr env (ExprMap.bindings map)

let typeof = typeof_no_memo ()
let back_typeof = back_typeof_no_memo ()
