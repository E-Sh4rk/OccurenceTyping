
open Types_additions
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
let parser_expr_to_expr tenv e =
    let rec aux env e =
        match e with
        | Const c -> Const c
        | Var str -> Var (StrMap.find str env)
        | Lambda (t,str,e) ->
            let varid = unique_varid () in
            let env = StrMap.add str varid env in
            Lambda (type_expr_to_typ tenv t, varid, aux env e)
        | Ite (e, t, e1, e2) ->
            Ite (aux env e, type_expr_to_typ tenv t, aux env e1, aux env e2)
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
