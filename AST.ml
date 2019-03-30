
type typ = Cduce.typ

type const =
    | Magic
    | Bool of bool
    | Int of int
    | Char of string

type var = string

type expr =
    | Const of const
    | Var of var
    | Lambda of typ * var * expr
    | Ite of expr * typ * expr * expr
    | App of expr * expr
    | Let of var * expr * expr

type dir =
    | LApp | RApp | RLet of var * expr

type path = dir list

module Expr = struct
    type t = expr
    let compare = compare
    let equiv t1 t2 = (compare t1 t2) = 0
end
module ExprMap = Map.Make(Expr)

exception Invalid_path

let rec follow_path e p =
    match e, p with
    | e, [] -> e
    | App (e,_), LApp::p
    | App (_,e), RApp::p -> follow_path e p
    | Let (_,_,e), (RLet _)::p -> follow_path e p
    | _ -> raise Invalid_path
