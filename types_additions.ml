
open Cduce

(* Construction of types *)

type type_base =
    TInt of int option * int option
    | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty

type type_expr =
| TBase of type_base
| TCustom of string
| TPair of type_expr * type_expr
| TArrow of type_expr * type_expr
| TCup of type_expr * type_expr
| TCap of type_expr * type_expr
| TDiff of type_expr * type_expr
| TNeg of type_expr

module StrMap = Map.Make(String)
type type_env = node StrMap.t

let empty_tenv = StrMap.empty

let type_base_to_typ t =
    match t with
    | TInt (lb,ub) -> Cduce.interval lb ub
    | TBool -> Cduce.bool_typ
    | TTrue -> Cduce.true_typ | TFalse -> Cduce.false_typ
    | TUnit -> Cduce.unit_typ | TChar -> Cduce.char_typ
    | TAny -> Cduce.any | TEmpty -> Cduce.empty

let type_expr_to_typ env t =
    let rec aux t =
        match t with
        | TBase tb -> cons (type_base_to_typ tb)
        | TCustom k -> StrMap.find k env
        | TPair (t1,t2) -> cons (mk_times (aux t1) (aux t2))
        | TArrow (t1,t2) -> cons (mk_arrow (aux t1) (aux t2))
        | TCup (t1,t2) ->
            let t1 = descr (aux t1) in
            let t2 = descr (aux t2) in
            cons (cup t1 t2)
        | TCap (t1,t2) ->
            let t1 = descr (aux t1) in
            let t2 = descr (aux t2) in
            cons (cap t1 t2)
        | TDiff (t1,t2) ->
            let t1 = descr (aux t1) in
            let t2 = descr (aux t2) in
            cons (diff t1 t2)
        | TNeg t -> cons (neg (descr (aux t)))
    in descr (aux t)

let define_atom env atom =
    let atom = String.capitalize_ascii atom in
    if StrMap.mem atom env
    then failwith (Printf.sprintf "Atom %s already defined!" atom)
    else StrMap.add atom (cons (mk_atom atom)) env

let define_types env defs =
    let declare_type env (name,_) =
        if StrMap.mem name env
        then failwith (Printf.sprintf "Type %s already defined!" name)
        else StrMap.add name (mk_new_typ ()) env
    in
    let env = List.fold_left declare_type env defs in
    let define_type (name,decl) =
        let t = type_expr_to_typ env decl in
        define_typ (StrMap.find name env) t
    in
    (* TODO: normalize? *)
    List.iter define_type defs ; env

let get_atom env atom =
    let atom = String.capitalize_ascii atom in
    if StrMap.mem atom env
    then descr (StrMap.find atom env)
    else failwith (Printf.sprintf "Atom %s not found!" atom)

(* Operations on types *)

let conj ts = List.fold_left cap any ts
let disj ts = List.fold_left cup empty ts

let square f out =
    let dnf = dnf f in
    let res = dnf |> List.map begin
        fun lst ->
            let lst = lst |> List.map begin
                fun (s,t) -> if is_empty (cap out t) then (s,false) else (s,true)
                (* Does not work with polymorphism (s should be instanciated...) *)
            end in
            let possibles = List.filter (function (_,b) -> b) lst |> List.map fst in
            let impossibles = List.filter (function (_,b) -> not b) lst |> List.map fst in
            diff (disj possibles) (disj impossibles)
    end in
    cap (domain f) (disj res)
