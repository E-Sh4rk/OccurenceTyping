
open Cduce
open Ast

let conj ts = List.fold_left cap any ts
let disj ts = List.fold_left cup empty ts

let square f out =
    let dnf = dnf f in
    let res = dnf |> List.map begin
        fun lst ->
            let res = lst |> List.map begin
                fun (s,t) ->
                    if is_empty (cap out t) then empty else s (* if polymorphism: s should be refined... *)
            end in
            List.fold_left cup empty res
    end in
    let res = List.fold_left cup empty res in
    cap (domain f) res
