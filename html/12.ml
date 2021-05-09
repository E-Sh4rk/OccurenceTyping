let incr = fun (Int -> Int) x -> magic
let charcode = fun (Char -> Int) x-> magic
let int_of_bool = fun (Bool -> Int) x -> magic


(* These define *)
atom number
atom character
atom boolean
atom undefined

type String = Number | Character | Boolean | Undefined

let typeof = fun (x:Any) ->
    if x is Int then number
    else if x is Char then character
    else if x is Bool then boolean
    else undefined

let test = fun (x:Any) ->
    if typeof x is Number then incr x
    else if typeof x is Character then charcode x
    else if typeof x is Boolean then int_of_bool x
    else 0