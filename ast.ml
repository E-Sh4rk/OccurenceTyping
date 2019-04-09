
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
type exprid = int

type annotation = int * Lexing.position

type ('a, 'typ, 'v) ast =
| Const of 'typ const
| Var of 'v
| Lambda of 'typ * 'v * ('a, 'typ, 'v) t
| RecLambda of 'v * 'typ * 'v * ('a, 'typ, 'v) t
| Ite of ('a, 'typ, 'v) t * 'typ * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| App of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Let of 'v * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Pair of ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Projection of projection * ('a, 'typ, 'v) t
| Debug of string * ('a, 'typ, 'v) t

and ('a, 'typ, 'v) t = 'a * ('a, 'typ, 'v) ast

type annot_expr = (annotation, typ, varid) t
type expr = (unit, typ, varid) t
type parser_expr = (annotation, type_expr, varname) t

module Expr = struct
    type t = expr
    let compare = compare
    let equiv t1 t2 = (compare t1 t2) = 0
end
module ExprMap = Map.Make(Expr)

type id_map = int StrMap.t

let empty_id_map = StrMap.empty

let unique_exprid =
    let last_id = ref 0 in
    fun _ -> (
        last_id := !last_id + 1 ;
        !last_id
    )

let unique_varid =
    let last_id = ref 0 in
    fun _ -> (
        last_id := !last_id + 1 ;
        !last_id
    )

let new_dummy_annot () =
    (unique_exprid (), Lexing.dummy_pos)

let parser_const_to_const tenv c =
    match c with
    | Atom t -> Atom (type_expr_to_typ tenv t)
    | Magic -> Magic
    | Unit -> Unit
    | Bool b -> Bool b
    | Int i -> Int i
    | Char c -> Char c

let parser_expr_to_annot_expr tenv id_map e =
    let rec aux env (a,e) =
        let e = match e with
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
        (a,e)
    in
    aux id_map e

let rec unannot (_,e) =
    let e = match e with
    | Const c -> Const c
    | Var v  -> Var v
    | Lambda (t, v, e) -> Lambda (t, v, unannot e)
    | RecLambda (s, t, v, e) -> RecLambda (s, t, v, unannot e)
    | Ite (e, t, e1, e2) -> Ite (unannot e, t, unannot e1, unannot e2)
    | App (e1, e2) -> App (unannot e1, unannot e2)
    | Let (v, e1, e2) -> Let (v, unannot e1, unannot e2)
    | Pair (e1, e2) -> Pair (unannot e1, unannot e2)
    | Projection (p, e) -> Projection (p, unannot e)
    | Debug (str, e) -> Debug (str, unannot e)
    in
    ( (), e )

exception Found
let rec substitute_var v ve (a,e) =
    try
    (
        let e = match e with
        | Const c -> Const c
        | Var v' when v=v' -> raise Found
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
        in
        (a,e)
    )
    with Found -> ve

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