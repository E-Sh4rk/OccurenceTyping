
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

type ('a, 'typ, 'v) t =
| Const of 'a * 'typ const
| Var of 'a * 'v
| Lambda of 'a * 'typ * 'v * ('a, 'typ, 'v) t
| RecLambda of 'a * 'v * 'typ * 'v * ('a, 'typ, 'v) t
| Ite of 'a * ('a, 'typ, 'v) t * 'typ * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| App of 'a * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Let of 'a * 'v * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Pair of 'a * ('a, 'typ, 'v) t * ('a, 'typ, 'v) t
| Projection of 'a * projection * ('a, 'typ, 'v) t
| Debug of 'a * string * ('a, 'typ, 'v) t

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
    let rec aux env e =
        match e with
        | Const (a,c) -> Const (a,parser_const_to_const tenv c)
        | Var (a,str) ->
            if StrMap.mem str env
            then Var (a,StrMap.find str env)
            else Const (a, Atom (get_atom tenv str))
        | Lambda (a,t,str,e) ->
            let varid = unique_varid () in
            let env = StrMap.add str varid env in
            Lambda (a,type_expr_to_typ tenv t, varid, aux env e)
        | RecLambda (a,recstr,t,str,e) ->
            let recvarid = unique_varid () in
            let varid = unique_varid () in
            let env = StrMap.add recstr recvarid env in
            let env = StrMap.add str varid env in
            RecLambda (a,recvarid, type_expr_to_typ tenv t, varid, aux env e)
        | Ite (a, e, t, e1, e2) ->
            Ite (a, aux env e, type_expr_to_typ tenv t, aux env e1, aux env e2)
        | App (a, e1, e2) ->
            App (a, aux env e1, aux env e2)
        | Let (a, str, e1, e2) ->
            let varid = unique_varid () in
            let env' = StrMap.add str varid env in
            Let (a, varid, aux env e1, aux env' e2)
        | Pair (a, e1, e2) ->
            Pair (a, aux env e1, aux env e2)
        | Projection (a, p, e) -> Projection (a, p, aux env e)
        | Debug (a, str, e) -> Debug (a, str, aux env e)
    in
    aux id_map e

let rec annot_expr_to_expr e =
    match e with
    | Const (_, c) -> Const ((), c)
    | Var (_, v)  -> Var ((), v)
    | Lambda (_, t, v, e) -> Lambda ((), t, v, annot_expr_to_expr e)
    | RecLambda (_, s, t, v, e) -> RecLambda ((), s, t, v, annot_expr_to_expr e)
    | Ite (_, e, t, e1, e2) -> Ite ((), annot_expr_to_expr e, t, annot_expr_to_expr e1, annot_expr_to_expr e2)
    | App (_, e1, e2) -> App ((), annot_expr_to_expr e1, annot_expr_to_expr e2)
    | Let (_, v, e1, e2) -> Let ((), v, annot_expr_to_expr e1, annot_expr_to_expr e2)
    | Pair (_, e1, e2) -> Pair ((), annot_expr_to_expr e1, annot_expr_to_expr e2)
    | Projection (_, p, e) -> Projection ((), p, annot_expr_to_expr e)
    | Debug (_, str, e) -> Debug ((), str, annot_expr_to_expr e)

let rec substitute_var v ve e =
    match e with
    | Const (a, c) -> Const (a, c)
    | Var (a, v') when v=v' -> ve
    | Var (a, v') -> Var (a, v')
    | Lambda (a, t, v', e) when v=v' -> Lambda (a, t, v', e)
    | Lambda (a, t, v', e) -> Lambda (a, t, v', substitute_var v ve e)
    | RecLambda (a, s, t, v', e) when v=v' || v=s -> RecLambda (a, s, t, v', e)
    | RecLambda (a, s, t, v', e) -> RecLambda (a, s, t, v', substitute_var v ve e)
    | Ite (a, e, t, e1, e2) -> Ite (a, substitute_var v ve e, t, substitute_var v ve e1, substitute_var v ve e2)
    | App (a, e1, e2) -> App (a, substitute_var v ve e1, substitute_var v ve e2)
    | Let (a, v', e1, e2) when v=v' -> Let (a, v', substitute_var v ve e1, e2)
    | Let (a, v', e1, e2) -> Let (a, v', substitute_var v ve e1, substitute_var v ve e2)
    | Pair (a, e1, e2) -> Pair (a, substitute_var v ve e1, substitute_var v ve e2)
    | Projection (a, p, e) -> Projection (a, p, substitute_var v ve e)
    | Debug (a, str, e) -> Debug (a, str, substitute_var v ve e)

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
