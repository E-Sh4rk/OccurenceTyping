
open Cduce
open Ast

type dir =
    | LApp | RApp | RLet of varid * expr

type path = dir list

exception Invalid_path

let rec follow_path e p =
    match e, p with
    | e, [] -> e
    | App (e,_), LApp::p
    | App (_,e), RApp::p -> follow_path e p
    | Let (v,e1,e2), (RLet _)::p -> follow_path (substitute_var v e1 e2) p
    | _ -> raise Invalid_path

type env = typ ExprMap.t
let empty_env = ExprMap.empty

let conj ts = List.fold_left cap any ts
let disj ts = List.fold_left cup empty ts

let square f out =
    let dnf = dnf f in
    let res = dnf |> List.map begin
        fun lst ->
            let res = lst |> List.map begin
                fun (s,t) ->
                    if is_empty (cap out t) then empty else s
            end in
            List.fold_left cup empty res
    end in
    let res = List.fold_left cup empty res in
    cap (domain f) res

exception Ill_typed

let rec all_paths_for_expr rev_prefix e =
    match e with
    | App (e1, e2) ->
        let p1 = all_paths_for_expr (LApp::rev_prefix) e1 in
        let p2 = all_paths_for_expr (RApp::rev_prefix) e2 in
        (List.rev rev_prefix)::(p1@p2)
    | Let (v,e1,e2) ->
        let p = all_paths_for_expr (RLet(v,e1)::rev_prefix) e2 in
        (List.rev rev_prefix)::p
    | Const _ | Var _ | Lambda _ | Ite _ -> [List.rev rev_prefix]

(* TODO: Add memoisation *)
(* TODO: Do substitution only once *)
let rec back_typeof_rev env e t p =
    match p with
    | [] -> t
    | LApp::p ->
        let dom = typeof env (follow_path e (List.rev (RApp::p))) in
        let codom = back_typeof_rev env e t p in
        mk_arrow (cons dom) (cons codom)
    | RApp::p ->
        let f_typ = typeof env (follow_path e (List.rev (LApp::p))) in
        let out_typ = back_typeof_rev env e t p in
        square f_typ out_typ
    | RLet(v,e1)::p ->
        back_typeof_rev env (substitute_var v e1 e) t p

and back_typeof env e t p =
    back_typeof_rev env e t (List.rev p)

and typeof env e =
    match ExprMap.find_opt e env with
    | Some t -> t
    | None ->
    begin match e with
        | Const Magic -> empty
        | Const (Bool _) -> bool_typ
        | Const (Int _) -> int_typ
        | Const (Char _) -> char_typ
        | Lambda (t,v,e) ->
            let dnf = dnf t in
            let rec valid_types acc dnf = match dnf with
            | [] -> acc
            | ts::dnf ->
                let is_valid (s,t) =
                    let new_env = ExprMap.add (Var v) s env in
                    subtype (typeof new_env e) t
                in
                if List.for_all is_valid ts then
                    let fs = List.map (fun (s,t) -> mk_arrow (cons s) (cons t)) ts in
                    valid_types ((conj fs)::acc) dnf
                else valid_types acc dnf
            in
            let ts = valid_types [] dnf in
            if ts = [] then raise Ill_typed else conj ts
        | App (e1, e2) ->
            let t1 = typeof env e1 in
            let t2 = typeof env e2 in
            apply t1 t2
        | Ite (e,t,e1,e2) ->
            let t0 = typeof env e in
            let env1 = refine_env env e (cap t0 t) in
            let env2 = refine_env env e (cap t0 (neg t)) in
            let t1 = typeof env1 e1 in
            let t2 = typeof env2 e2 in
            cup t1 t2
        | Let (v, e1, e2) ->
            let env = ExprMap.add e1 (typeof env e1) env in
            typeof env (substitute_var v e1 e2)
        | Var _ -> failwith "Unknown variable type..."
    end

and refine_env env e t =
    let paths = all_paths_for_expr [] e in
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
        let old_type = typeof env e' in
        let refine acc p =
            cap acc (back_typeof env e t p)
        in
        let new_type = List.fold_left refine old_type paths in
        ExprMap.add e' new_type acc
    in
    List.fold_left refine_for_expr env (ExprMap.bindings map)
