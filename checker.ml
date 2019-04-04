
open Cduce
open Ast
open Types_additions

type dir =
    | LApp | RApp | LPair | RPair | PFst | PSnd | Dbg of string

type path = dir list

exception Invalid_path

let rec follow_path e p =
    match e, p with
    | e, [] -> e
    | App (e,_), LApp::p
    | App (_,e), RApp::p
    | Pair (e,_), LPair::p
    | Pair (_,e), RPair::p
    | Projection (Fst, e), PFst::p
    | Projection (Snd, e), PSnd::p
    | Debug (_, e), (Dbg _)::p
    -> follow_path e p
    (* Don't forget to add things here! *)
    | _ -> raise Invalid_path

type env = typ ExprMap.t
let empty_env = ExprMap.empty
let is_bottom env =
    let is_bottom (_,v) = is_empty v in
    List.exists is_bottom (ExprMap.bindings env)


exception Ill_typed

module IntMap = Map.Make(struct type t = int let compare = compare end)

let eliminate_all_lets e =
    let rec aux env e =
        match e with
        | Const c -> Const c
        | Var v when IntMap.mem v env -> IntMap.find v env
        | Var v -> Var v
        | Lambda (t, v, e) -> Lambda (t, v, aux (IntMap.remove v env) e)
        | Ite (e, t, e1, e2) -> Ite (aux env e, t, aux env e1, aux env e2)
        | App (e1, e2) -> App (aux env e1, aux env e2)
        | Let (v, e1, e2) ->
            let e1 = aux env e1 in
            Let (v, e1, aux (IntMap.add v e1 env) e2)
        | Pair (e1, e2) -> Pair (aux env e1, aux env e2)
        | Projection (p, e) -> Projection (p, aux env e)
        | Debug (str, e) -> Debug (str, aux env e)
    in
    aux IntMap.empty e

let rec all_paths_for_expr rev_prefix e =
    match e with
    | App (e1, e2) ->
        let p1 = all_paths_for_expr (LApp::rev_prefix) e1 in
        let p2 = all_paths_for_expr (RApp::rev_prefix) e2 in
        (List.rev rev_prefix)::(p1@p2)
    | Let _ -> failwith "Let bindings in tests must be eliminated before back-typing!"
    | Pair (e1, e2) ->
        let p1 = all_paths_for_expr (LPair::rev_prefix) e1 in
        let p2 = all_paths_for_expr (RPair::rev_prefix) e2 in
        (List.rev rev_prefix)::(p1@p2)
    | Projection (Fst, e) ->
        (List.rev rev_prefix)::(all_paths_for_expr (PFst::rev_prefix) e)
    | Projection (Snd, e) ->
        (List.rev rev_prefix)::(all_paths_for_expr (PSnd::rev_prefix) e)
    | Debug (str, e) ->
        (List.rev rev_prefix)::(all_paths_for_expr ((Dbg str)::rev_prefix) e)
    | Const _ | Var _ | Lambda _ | Ite _ -> [List.rev rev_prefix]

module PathMap = Map.Make(struct type t = path let compare = compare end)

let rec back_typeof_rev self (memo_t, env, e, t, p) =
    let typeof = typeof_memo memo_t in
    let typeof p = typeof env (follow_path e (List.rev p)) in
    let self = fun p -> self (memo_t, env, e, t, p) in
    let t = match p with
    | [] -> t
    | LApp::p -> mk_arrow (cons (typeof (RApp::p))) (cons (self p))
    | RApp::p -> square (typeof (LApp::p)) (self p)
    | LPair::p -> pi1 (self p)
    | RPair::p -> pi2 (self p)
    | PFst::p -> mk_times (cons (self p)) any_node
    | PSnd::p -> mk_times any_node (cons (self p))
    | (Dbg str)::p -> let res = self p in Format.printf "%s (back_typeof): " str ; Utils.print_type res; res
    in
    cap t (typeof p)

and back_typeof_no_memo _ =
    let back_typeof_rev = Utils.do_not_memoize back_typeof_rev in
    fun env e t p -> back_typeof_rev (Hashtbl.create 1, env, e, t, List.rev p)

and optimized_back_typeof env e t ps =
    let n = List.length ps in
    let path_select (_,_,_,_,p) = p in
    let back_typeof_rev = Utils.memoize back_typeof_rev path_select (Hashtbl.create n) in
    let memo_to = Hashtbl.create n in
    List.map (fun p -> back_typeof_rev (memo_to, env, e, t, List.rev p)) ps

and typeof_raw self (env, e) =
    (* The rule that states that 'every expression has type bottom in the bottom environment'
       is integrated in the Ite case for efficiency reasons. *)
    match ExprMap.find_opt e env with
    | Some t -> t
    | None ->
    begin match e with
        | Const Magic -> empty
        | Const (Bool _) -> bool_typ
        | Const (Int _) -> int_typ
        | Const (Char _) -> char_typ
        | Const Unit -> unit_typ
        | Lambda (t,v,e) ->
            let dnf = dnf t in
            let rec valid_types acc dnf = match dnf with
            | [] -> acc
            | ts::dnf ->
                let is_valid (s,t) =
                    let new_env = ExprMap.add (Var v) s env in
                    subtype (self (new_env, e)) t
                in
                if List.for_all is_valid ts then
                    let fs = List.map (fun (s,t) -> mk_arrow (cons s) (cons t)) ts in
                    valid_types ((conj fs)::acc) dnf
                else valid_types acc dnf
            in
            let ts = valid_types [] dnf in
            if ts = [] then raise Ill_typed else conj ts
        | App (e1, e2) ->
            let t1 = self (env, e1) in
            let t2 = self (env, e2) in
            if subtype t2 (domain t1) then apply t1 t2 else raise Ill_typed
        | Ite (e,t,e1,e2) ->
            (* No need to check the type of e here: it is already checked in refine_env *)
            let env1 = refine_env env e t in
            let env2 = refine_env env e (neg t) in
            let t1 = if is_bottom env1 then empty else self (env1, e1) in
            let t2 = if is_bottom env2 then empty else self (env2, e2) in
            cup t1 t2
        | Let (v, e1, e2) ->
            let env = ExprMap.add e1 (self (env, e1)) env in
            self (env, substitute_var v e1 e2)
        | Var _ -> failwith "Unknown variable type..."
        | Pair (e1, e2) ->
            let t1 = self (env, e1) in
            let t2 = self (env, e2) in
            mk_times (cons t1) (cons t2)
        | Projection (Fst, e) ->
            let t = self (env, e) in
            if subtype t pair_any then pi1 t else raise Ill_typed
        | Projection (Snd, e) ->
            let t = self (env, e) in
            if subtype t pair_any then pi2 t else raise Ill_typed
        | Debug (str, e) ->
            let res = self (env, e) in
            Format.printf "%s (typeof): " str ; Utils.print_type res ;
            res
    end

and typeof_no_memo _ =
    let typeof = Utils.do_not_memoize typeof_raw in
    fun env e -> typeof (env, e)

and typeof_memo ht =
    let typeof = Utils.memoize typeof_raw snd ht in
    fun env e -> typeof (env, e)

and refine_env env e t =
    (* No need to continue memoisation here because back_typeof never goes into Ite *)
    let e = eliminate_all_lets e in
    let paths = all_paths_for_expr [] e in
    (* Deduce a type for each path *)
    let paths_t = optimized_back_typeof env e t paths in
    let paths_t = List.fold_left2 (fun acc p t -> PathMap.add p t acc) PathMap.empty paths paths_t in 
    (* Build a map from sub-expressions (occurrences) to paths *)
    let add_path acc p =
        let e = follow_path e p in
        let v = match ExprMap.find_opt e acc with
        | None -> [p]
        | Some ps -> p::ps
        in ExprMap.add e v acc
    in
    let map = List.fold_left add_path ExprMap.empty paths in
    (* Refine the type for each expression *)
    let refine_for_expr acc (e', paths) =
        let types = List.map (fun p -> PathMap.find p paths_t) paths in
        ExprMap.add e' (conj types) acc
    in
    List.fold_left refine_for_expr env (ExprMap.bindings map)

let typeof = typeof_no_memo ()
let back_typeof = back_typeof_no_memo ()
