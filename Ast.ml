
open Types_additions
type typ = Cduce.typ

type 't const =
| Magic
| Unit
| Bool of bool
| Int of int
| Char of char
| Atom of 't

type projection = Fst | Snd

type varname = string
type varid = int (* It is NOT De Bruijn indexes, but unique IDs *)

type ('t, 'v) expr' =
| Const of 't const
| Var of 'v
| Lambda of 't * 'v * ('t, 'v) expr'
| RecLambda of 'v * 't * 'v * ('t, 'v) expr'
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

type id_map = int StrMap.t

let empty_id_map = StrMap.empty

let unique_varid =
    let last_id = ref 0 in
    fun _ -> (
        last_id := !last_id + 1 ;
        !last_id
    )

let parser_const_to_const tenv c =
    match c with
    | Atom t -> Atom (type_expr_to_typ tenv t)
    | Magic -> Magic
    | Unit -> Unit
    | Bool b -> Bool b
    | Int i -> Int i
    | Char c -> Char c

let parser_expr_to_expr tenv id_map e =
    let rec aux env e =
        match e with
        | Const c -> Const (parser_const_to_const tenv c)
        | Var str ->
            if StrMap.mem str env
            then Var (StrMap.find str env)
            else Const (Atom (get_atom tenv str))
        | Lambda (t,str,e) ->
            let varid = unique_varid () in
            let env = StrMap.add str varid env in
            Lambda (type_expr_to_typ tenv t, varid, aux env e)
        | RecLambda (recstr,t,str,e) ->
            let recvarid = unique_varid () in
            let varid = unique_varid () in
            let env = StrMap.add recstr recvarid env in
            let env = StrMap.add str varid env in
            RecLambda (recvarid, type_expr_to_typ tenv t, varid, aux env e)
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
    aux id_map e

let rec substitute_var v ve e =
    match e with
    | Const c -> Const c
    | Var v' when v=v' -> ve
    | Var v' -> Var v'
    | Lambda (t, v', e) when v=v' -> Lambda (t, v', e)
    | Lambda (t, v', e) -> Lambda (t, v', substitute_var v ve e)
    | RecLambda (s, t, v', e) when v=v' || v=s -> RecLambda (s, t, v', e)
    | RecLambda (s, t, v', e) -> RecLambda (s, t, v', substitute_var v ve e)
    | Ite (e, t, e1, e2) -> Ite (substitute_var v ve e, t, substitute_var v ve e1, substitute_var v ve e2)
    | App (e1, e2) -> App (substitute_var v ve e1, substitute_var v ve e2)
    | Let (v', e1, e2) when v=v' -> Let (v', substitute_var v ve e1, e2)
    | Let (v', e1, e2) -> Let (v', substitute_var v ve e1, substitute_var v ve e2)
    | Pair (e1, e2) -> Pair (substitute_var v ve e1, substitute_var v ve e2)
    | Projection (p, e) -> Projection (p, substitute_var v ve e)
    | Debug (str, e) -> Debug (str, substitute_var v ve e)

let const_to_typ c =
    match c with
    | Magic -> Cduce.empty
    | Bool true -> Cduce.true_typ
    | Bool false -> Cduce.false_typ
    | Int i -> Cduce.interval (Some i) (Some i)
    | Char c -> Cduce.single_char c
    | Unit -> Cduce.unit_typ
    | Atom t -> t

type parser_element =
| Definition of (string * parser_expr)
| Atoms of string list
| Types of (string * type_expr) list

type parser_program = parser_element list
