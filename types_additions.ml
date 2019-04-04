
open Cduce
open Ast

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
