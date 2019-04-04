
open Cduce
open Ast

let conj ts = List.fold_left cap any ts
let disj ts = List.fold_left cup empty ts

let square f out =
    let dnf = dnf f in
    let res = dnf |> List.map begin
        fun lst ->
            let lst = lst |> List.map begin
                fun (s,t) ->
                    if is_empty (cap out t) then (s,false) else (s,true) (* if polymorphism: s should be refined... *)
            end in
            let possibles = List.filter (function (_,b) -> b) lst |> List.map fst in
            let impossibles = List.filter (function (_,b) -> not b) lst |> List.map (fun (t,_) -> neg t) in
            cap (disj possibles) (conj impossibles)
    end in
    cap (domain f) (disj res)
