
open Cduce
open Ast

type env = typ ExprMap.t

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

and back_typeof env e t p =
    back_typeof_rev env e t (List.rev p)

and typeof env e =
    failwith "TODO"
