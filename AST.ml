
type typ = Cduce.typ

type const =
    | Magic
    | Bool of bool
    | Int of int
    | Char of char

type varname = string
type varid = int

type 'var expr' =
    | Const of const
    | Var of 'var
    | Lambda of typ * 'var * 'var expr'
    | Ite of 'var expr' * typ * 'var expr' * 'var expr'
    | App of 'var expr' * 'var expr'
    | Let of 'var * 'var expr' * 'var expr'

type parser_expr = varname expr'
type expr = varid expr'

type dir =
    | LApp | RApp | RLet of varid * expr

type path = dir list

let rec make_lambda_abstraction vars t e =
    match vars with
    | [] -> e
    | [x] -> Lambda (t,x,e)
    | x::vars ->
        let new_t = Cduce.apply t (Cduce.domain t) in
        Lambda (t,x,make_lambda_abstraction vars new_t e)

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

let parser_expr_to_expr e =
    let rec aux env e =
        match e with
        | Const c -> Const c
        | Var str -> Var (StrMap.find str env)
        | Lambda (t,str,e) ->
            let varid = unique_varid () in
            let env = StrMap.add str varid env in
            Lambda (t, varid, aux env e)
        | Ite (e, t, e1, e2) ->
            Ite (aux env e, t, aux env e1, aux env e2)
        | App (e1, e2) ->
            App (aux env e1, aux env e2)
        | Let (str, e1, e2) ->
            let varid = unique_varid () in
            let env = StrMap.add str varid env in
            Let (varid, aux env e1, aux env e2)
    in
    aux StrMap.empty e

exception Invalid_path

let rec follow_path e p =
    match e, p with
    | e, [] -> e
    | App (e,_), LApp::p
    | App (_,e), RApp::p -> follow_path e p
    | Let (_,_,e), (RLet _)::p -> follow_path e p
    | _ -> raise Invalid_path
