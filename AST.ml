
type typ = Cduce.typ

type const =
    | Magic
    | Unit
    | Bool of bool
    | Int of int
    | Char of char

type projection = Fst | Snd

type varname = string
type varid = int (* It is NOT De Bruijn indexes, but unique IDs *)

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

type ('t, 'v) expr' =
| Const of const
| Var of 'v
| Lambda of 't * 'v * ('t, 'v) expr'
| Ite of ('t, 'v) expr' * 't * ('t, 'v) expr' * ('t, 'v) expr'
| App of ('t, 'v) expr' * ('t, 'v) expr'
| Let of 'v * ('t, 'v) expr' * ('t, 'v) expr'
| Pair of ('t, 'v) expr' * ('t, 'v) expr'
| Projection of projection * ('t, 'v) expr'
| Debug of string * ('t, 'v) expr'

type parser_expr = (type_expr, varname) expr'
type expr = (typ, varid) expr'

module Expr = struct
    type t = expr
    let compare = compare
    let equiv t1 t2 = (compare t1 t2) = 0
end
module ExprMap = Map.Make(Expr)

let unique_varid =
    let last_id = ref 0 in
    fun _ -> (
        last_id := !last_id + 1 ;
        !last_id
    )

module StrMap = Map.Make(String)

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

let parser_expr_to_expr e =
    let rec aux env e =
        match e with
        | Const c -> Const c
        | Var str -> Var (StrMap.find str env)
        | Lambda (t,str,e) ->
            let varid = unique_varid () in
            let env = StrMap.add str varid env in
            Lambda (type_expr_to_typ t, varid, aux env e)
        | Ite (e, t, e1, e2) ->
            Ite (aux env e, type_expr_to_typ t, aux env e1, aux env e2)
        | App (e1, e2) ->
            App (aux env e1, aux env e2)
        | Let (str, e1, e2) ->
            let varid = unique_varid () in
            let env' = StrMap.add str varid env in
            Let (varid, aux env e1, aux env' e2)
        | Pair (e1, e2) ->
            Pair (aux env e1, aux env e2)
        | Projection (p, e) -> Projection (p, aux env e)
        | Debug (str, e) -> Debug (str, aux env e)
    in
    aux StrMap.empty e

let rec substitute_var v ve e =
    match e with
    | Const c -> Const c
    | Var v' when v=v' -> ve
    | Var v' -> Var v'
    | Lambda (t, v', e) when v=v' -> Lambda (t, v', e)
    | Lambda (t, v', e) -> Lambda (t, v', substitute_var v ve e)
    | Ite (e, t, e1, e2) -> Ite (substitute_var v ve e, t, substitute_var v ve e1, substitute_var v ve e2)
    | App (e1, e2) -> App (substitute_var v ve e1, substitute_var v ve e2)
    | Let (v', e1, e2) when v=v' -> Let (v', substitute_var v ve e1, e2)
    | Let (v', e1, e2) -> Let (v', substitute_var v ve e1, substitute_var v ve e2)
    | Pair (e1, e2) -> Pair (substitute_var v ve e1, substitute_var v ve e2)
    | Projection (p, e) -> Projection (p, substitute_var v ve e)
    | Debug (str, e) -> Debug (str, substitute_var v ve e)

type parser_element =
| Definition of (string * parser_expr)
| Atoms of string list
| Types of (string * type_expr) list

type parser_program = parser_element list
