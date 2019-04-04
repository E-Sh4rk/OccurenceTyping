
open Cduce

(* Construction of types *)

type type_base =
    TInt | TBool | TTrue | TFalse | TUnit | TChar | TAny | TEmpty

type type_expr =
| TBase of type_base
| TCustom of string
| TPair of type_expr * type_expr
| TArrow of type_expr * type_expr
| TCup of type_expr * type_expr
| TCap of type_expr * type_expr
| TDiff of type_expr * type_expr
| TNeg of type_expr

let type_base_to_typ t =
    match t with
    | TInt -> Cduce.int_typ | TBool -> Cduce.bool_typ
    | TTrue -> Cduce.true_typ | TFalse -> Cduce.false_typ
    | TUnit -> Cduce.unit_typ | TChar -> Cduce.char_typ
    | TAny -> Cduce.any | TEmpty -> Cduce.empty

let rec type_expr_to_typ t =
    match t with
    | TBase tb -> type_base_to_typ tb
    | TCustom _ -> failwith "TODO"
    | TPair (t1,t2) ->
        (* /!\ Do not support recursive types yet. TODO *)
        let t1 = type_expr_to_typ t1 in
        let t2 = type_expr_to_typ t2 in
        Cduce.mk_times (Cduce.cons t1) (Cduce.cons t2)
    | TArrow (t1,t2) ->
        (* /!\ Do not support recursive types yet. TODO *)
        let t1 = type_expr_to_typ t1 in
        let t2 = type_expr_to_typ t2 in
        Cduce.mk_arrow (Cduce.cons t1) (Cduce.cons t2)
    | TCup (t1,t2) ->
        let t1 = type_expr_to_typ t1 in
        let t2 = type_expr_to_typ t2 in
        Cduce.cup t1 t2
    | TCap (t1,t2) ->
        let t1 = type_expr_to_typ t1 in
        let t2 = type_expr_to_typ t2 in
        Cduce.cap t1 t2
    | TDiff (t1,t2) ->
        let t1 = type_expr_to_typ t1 in
        let t2 = type_expr_to_typ t2 in
        Cduce.diff t1 t2
    | TNeg t -> Cduce.neg (type_expr_to_typ t)

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
